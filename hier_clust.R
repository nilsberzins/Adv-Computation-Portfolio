
#' Agglomerative Hierarchical Clustering Method`
#'
#' @param data A data frame
#' @param method A string that states which distance method the user wants to use
#'
#' @return A list with clustering path
#'
#' @export
#'
hier_clust <- function(data, method = "euclidean") {

    avail_methods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

    if(!(method %in% avail_methods)) {
        stop("Please put your distance calculation method in all lower case")
    }

    n <- nrow(data)

    iterations <- 0 #will count the height of the dendrogram by adding 1 to each
    distances <- as.matrix(dist(data, method = method, diag = TRUE, upper = TRUE)) #distance matrix

    clusters <- list() #the list from which the clusters will be stored and manipulated
    for(i in 1:n) {
        clusters[[i]] <- i
    }

    while(length(clusters) > 1) { #keep iterating until there is only one cluster left

        min_dist <- Inf
        merge_index <- NULL

        for(i in 1:nrow(distances)) {
            for(j in 1:nrow(distances)) {

                if(i == j) {

                } else {

                    # cluster1 <- clusters[[i]] #pulls the index of the cluster
                    # cluster2 <- clusters[[j]]

                    dist_val <- distances[i, j] #puts the indices of the clusters into the dist matrix

                    if(all(dist_val < min_dist)) { #Sets the smallest distance between two points by finding the smallest distance value
                        min_dist <- dist_val
                        merge_index <- c(i, j) #index in clusters
                    }
                }

            }

        }

        merged_cluster <- c(clusters[[merge_index[1]]], clusters[[merge_index[2]]]) #create new cluster that adds in the min dist cluster pairing
        clusters <- clusters[-merge_index] #deletes the indexes of both clusters in the pair
        clusters <- c(clusters, list(merged_cluster)) #puts the new cluster back into the clusters list

        #Average Linkage

        for(i in 1:nrow(distances)) {

            avg_dist <- sum(distances[merge_index[1], i], distances[merge_index[2], i]) / 2
            distances[merge_index[1], i] <- avg_dist
            distances[i, merge_index[1]] <- avg_dist
        }

        distances <- distances[-merge_index[2], -merge_index[2]]

    }

    iterations <- iterations + 1

    return(clusters[[1]])

}
