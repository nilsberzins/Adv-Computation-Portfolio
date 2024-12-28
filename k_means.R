#' Implements K means by hands
#'
#' @param dat A data set
#' @param k The number of desired clusters
#' @param pca Boolean value indicating whether or not to run Principal Component Analysis prior to K means
#' @param k_means_plus Boolean value indicating whether or not to choose the initial clusters in a "smart" way
#'
#' @return Returns cluster assignments, the mean centroid of each cluster, the number of iterations to
#' achieve the clustering, and the total sum of squares of the data to the mean centroid of the data set.
#'
#' @import dplyr
#' @import stats
#'
#' @export

k_means <- function(dat, k, pca = FALSE, k_means_plus = TRUE) {

    #Taking only numeric data
    dat <- dat %>%
        select(. ,where(is.numeric))

    # Automatically perform PCA before clustering
    if (pca == TRUE) {
        dat_pca <- dat %>% select(c(1:2)) %>% princomp()
        dat_pc_scores <- dat_pca$scores %>% as.data.frame()
        dat <- dat_pc_scores
    }

    #Number of rows/observations
    num_obs <- nrow(dat)

    if(k_means_plus == TRUE) {

        centers <- k_means_plus_plus(dat, k)

    } else {

        #Randomly select K observations
        random_obs <- sample(num_obs, size = k)
        #Create centers for reference
        centers <- slice(dat, random_obs)

    }

    #Vectors of clusters
    clusters <- c(1)
    last_clusters <- c(0)
    smallest_distances <- c(0)
    dist <- c(0)
    iterations <- 0

    #Center point of whole data set for Total Sum of Squares calculation
    center_of_dat <- colMeans(dat)

    #Repeat until we end up on the same cluster
    repeat {

        #Loop to calculate Euclidean distance of each point from the randomly selected centers
        for(i in 1:num_obs) {

            #Attaching observation to data frame of the randomly chosen k-clusters
            center_and_point <- dat[i, ] %>% rbind(centers)

            #Measuring Euclidean distance between observation and k-clusters
            dist <- dist(center_and_point, method = "euclidean", upper = FALSE)

            #Taking smallest distance out of the measurements for each k-clusters
            closest_cluster <- dist[1:k] %>%
                which.min()

            #Pulling smallest distance for sum of squares
            smallest_distances[i] <- dist[closest_cluster]

            #Assigning cluster number to observation
            clusters[i] <- closest_cluster

        }

        #Checking to make sure the cluster arraignment updated
        if(all(last_clusters == clusters)) {
            break
        }

        #Setting last cluster value
        last_clusters <- clusters

        #Finding new centers by taking mean of data points in each k-cluster, then removing cluster column
        if(k_means_plus == FALSE) {

            centers <- dat %>%
                cbind(clusters) %>%
                group_by(clusters) %>%
                summarize_all(mean) %>%
                select(-clusters)

        }

        #Counting number of iterations to get repeated k-means clustering
        iterations <- iterations + 1

    }

    #Attaching cluster assignment to original data frame
    cluster_assignments <- dat %>%
        cbind(clusters) %>%
        cbind(smallest_distances)

    #Total Sum of Squares
    TSS <- total_sum_squares(center_of_dat, dat)

    #Output cluster assignments, total sum of squares, at the minimum
    results <- list("Clusterings" = cluster_assignments,
                    "Cluster Means" = centers,
                    "Iterations" = iterations,
                    "Total Sum of Squares" = TSS)

    return(results)

}


#' Plots the results of k_means() clustering in the first two PC dimensions
#'
#' @param cluster_assignments Clustering from k_means()
#'
#' @return A plot
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export

plot_clusterings <- function(cluster_assignments) {

    if (!all(names(cluster_assignments)[1:3] == c("Comp.1", "Comp.2", "clusters"))) {
        stop("Pass $Clusterings of k_means(dat, k, pca = TRUE)")
    } else if (nrow(cluster_assignments) == 0) {
        stop("cluster_assignments should have more than 0 rows")
    }

    cluster_assignments %>%
        as.data.frame() %>%
        mutate(clusters = as.factor(.data$clusters)) %>%
        ggplot(aes(
            x = .data$Comp.1,
            y = .data$Comp.2,
            color = .data$clusters,
            shape = .data$clusters,
            group = .data$clusters
        )) +
        geom_point(size = 3) +
        stat_ellipse() +
        theme_minimal()
}


#' Retrieves Total Sum of Squares
#'
#' @param center The center point of the data that all Euclidean distances will be calculated on
#' @param dat A data set
#'
#' @return Total Sum of Squares for the distances from mean data point in the data set to all observations

total_sum_squares <- function (center, dat) {

   results <-  Rfast::total.dista(x = as.matrix(dat), y = matrix(center, nrow = 1))

   return(results)

}

#' Run K-means ++ initialization method to determine initial centroid points
#'
#' @param dat A data set
#' @param k Number of clusters
#'
#' @import dplyr
#'
#' @return Three points in the data set that will start as the inital centroids for each cluster

k_means_plus_plus <- function(dat, k) {

    #Taking only numeric data
    dat <- dat %>%
        select(. ,where(is.numeric))

    #Number of rows/observations
    num_obs <- nrow(dat)

    #Randomly select 1st centroid
    random_obs <- sample(num_obs, size = 1)
    #Pull out first centroid
    center <- slice(dat, random_obs)

    #Vectors of distance
    dist <- c(0)
    centroids <- data.frame()

    while (nrow(centroids) < k) {

        #Loop to calculate Euclidean distance of each point from the 1st centroid
        for(i in 1:num_obs) {

            #Attaching observation to data frame of the randomly chosen k-clusters
            center_and_point <- dat[i, ] %>% rbind(center)

            #Measuring Euclidean distance between observation and k-clusters
            dist[i] <- dist(center_and_point, method = "euclidean", upper = FALSE)

        }

        #Creating vector of weight probabilities, so furthest distance gets higher probability of being picked
        probs <- dist^2 / sum(dist^2)

        #Selecting next centroid using probability distribution
        next_centroid <- dat[sample(nrow(dat), 1, prob = probs), ]

        #Add the selected centroid to the list of centroids
        centroids <- rbind(centroids, next_centroid)

    }

    return(centroids)

}
