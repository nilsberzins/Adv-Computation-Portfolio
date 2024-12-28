#' Forms clusters by picking a random observation and finding points that are within a specified distance to that center point
#'
#' @param dat A data set
#' @param distance The radius around a center point where the function will look for points to add to it's cluster
#'
#' @return Returns cluster assignments
#'
#' @import dplyr
#' @importFrom dplyr select mutate_all
#' @import stats
#'
#' @export

AbstractCluster <- function(dat, distance = 4) {

  #Taking only numeric data
  dat <- dat %>%
    select(. ,where(is.numeric)) %>%
    mutate_all(scale) %>%
    tibble::rowid_to_column("row_index")

  #Copying data set
  final_dat <- dat

  #Number of rows/observations for later check
  original_length <- nrow(dat)
  num_obs <- nrow(dat)

  #Counters
  cluster <- 1

  repeat {

    #If on second round of cluster assignment, then subset original dataframe to take out observations that have been assigned clusters
    if(cluster > 1) {

      dat <- dat[!(dat$row_index %in% rows_to_exclude$row_index), ]
      num_obs <- nrow(dat)
      row_indices <- dat$row_index

    }

    #Randomly select 1 observations
    random_obs <- sample(num_obs, size = 1)

    #Create starting center for reference
    starting_center <- slice(dat, random_obs)

    #Reset distance and cluster_assignment vectors so they keep the right length
    dist <- c(0)
    cluster_assignment <- c(0)
    core_points <- c(0)

    #Loop to calculate Euclidean distance of each point from the randomly selected centers
    for(i in 1:num_obs) {

      #Attaching observation to data frame of the randomly chosen center
      center_and_point <- dat[i, ] %>% rbind(starting_center)

      #Measuring Euclidean distance between observation and randomly chosen center
      dist[i] <- dist(center_and_point, method = "euclidean", upper = FALSE)

    }

    #Test if distance between points and center_point is within distance declared by user
    core_points <- (dist < distance)

    #If TRUE, then assign those observation's row value to the cluster
    cluster_assignment[core_points == TRUE] <- cluster

    #Add cluster assignments to final data set
    if(cluster == 1) {

      final_dat$clusters <- cluster_assignment

    } else {

      final_dat <- final_dat %>%
        mutate(clusters = replace(clusters, row_indices, cluster_assignment))

    }

    #Creating data frame of rows to exclude in next iteration
    rows_to_exclude <- final_dat %>%
      tidyr::drop_na() %>%
      select(.data$row_index)

    #Add one to cluster to indicate next iteration of clustering
    cluster <- cluster + 1

    #If all of the cluster assignment has been
    if(nrow(dat) <= 1) {
      break
    }

  }

  return(final_dat)

}

#' Cluster plot
#'
#' @param clusters Clusters from AbstractCluster
#'
#' @import dplyr
#'
#' @return plot
#'
#' @export

ClusterPlot <- function(abstract_clusters) {
  abstract_clusters %>%
    mutate(clusters = as.numeric(clusters)) %>%
    ggplot(aes(x = row_indices, y = clusters)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Clusters",
         subtitle = "Distribution of clusters")
}


#' Creates an Elbow Method Graph to help the user manually choose # of clusters necessary for their k-means calculation.
#'
#' @param dat A data set
#' @param explanatory explanatory variable of interest
#' @param response response variable of interest
#' @param kmax the maximum number of clusters the program should run and graph
#'
#' @import dplyr
#' @import maotai
#' @import purrr
#' @import ggplot2
#' @import plotly
#'
#' @return Elbow Method Graph
#'
#' @export
CreateElbowGraph <- function(dat, explanatory, response, kmax) {

  if(kmax > nrow(dat)) {
    stop("The max number of clusters cannot exceed the number of observations in the datatable")
  }

  if(kmax <= 0) {
    stop("The max number of clusters must be a positive, non-zero number that is less than, or equal to, the numbers of observations in the datatable")
  }

  elbow_dat <- dat |>
    select({{explanatory}}, {{response}}) |>
    as.matrix()

  #within cluster sum of squares for each cluster combination until kmax
  wcss <- c()

  for(i in 2:kmax) { #cannot start at 1 when using the kmeanspp function

    #find cluster placements
    clusters <- data.frame(clusters = kmeanspp(elbow_dat, i))

    #create index values to help track the original values after clustering
    obs_num <- seq(nrow(dat))
    clusters <- cbind(obs_num, clusters)

    #split the df into k dfs each representing one cluster
    split_clust <- split(clusters, clusters$clusters)

    #call helper function to get original value of each datapoint in each cluster
    og_values <- map(split_clust, ~ getOriginalValues(.x$obs_num,
                                                      data.frame(elbow_dat),
                                                      {{explanatory}},
                                                      {{response}}))

    #return within cluster SS for each cluster
    cluster_ss <- map_int(og_values, ~getWCSS(.x))

    #add this this cluster specification's wcss to the running wcss calc
    wcss <- c(wcss, sum(cluster_ss))
  }

  #adding in index values to indicate the # of clusters used and changing column names
  wcss <- cbind(seq(from = 2, to = length(wcss) + 1), wcss)

  colnames(wcss)[1] <- "Clusters"
  colnames(wcss)[2] <- "WCSS"

  #graphing the elbow method
  data.frame(wcss) |>
    plot_ly(x = ~Clusters, y = ~WCSS, type = 'scatter', mode = 'lines+markers')

}

#' Helper function for CreateElbowGraph that grabs the original data values from the indices provided for each datapoin in every cluster
#'
#' @param indices A vector holding the indices of a cluster's points
#' @param elbow_dat Original data frame
#' @param explanatory Explanatory variable of interest
#' @param response Response variable of interest
#'
#' @return A data frame that represents the true values of each point in a cluster

getOriginalValues <- function(indices, elbow_dat, explanatory, response) {

  #merging the indices df with the original data so that each cluster df has correct values attributed to them
  combined_cluster_og <- merge(data.frame(obs_num = indices), elbow_dat, by.x = "obs_num", by.y = "row.names", all.x = TRUE)
  og_values <- combined_cluster_og[, c(explanatory, response), drop = FALSE]

  return(og_values)

}

#' Helper function for CreateElbowGraph that calculates the within cluster sum of squares
#'
#' @param clusters a data frame that represents a cluster
#'
#' @return integer describing a cluster's sum of squares from the centroid

getWCSS <- function(clusters) {

  #as per the structure of the dfs returned by getOriginalValues(...), the explanatory var will be in column 1 and reponse in column 2
  centroid <- c(mean(clusters[[1]]), mean(clusters[[2]]))

  #preparing df for distance calculation
  clust_matrix <- as.matrix(clusters)
  centroid_clust <- rbind(centroid, clust_matrix)

  #euclidean distances matrix
  distances <- dist(centroid_clust)

  #selecting just the distances from the centroid
  dist_vec <- as.numeric(distances[1:nrow(clusters)])

  #squaring distances and summing to get the wcss
  dist_squared_sum <- sum(dist_vec^2)

  return(as.integer(dist_squared_sum))

}
