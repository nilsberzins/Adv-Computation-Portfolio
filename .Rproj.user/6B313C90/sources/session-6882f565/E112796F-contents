
#' Agglomerative Hierarchical Clustering Method`
#'
#' @param data A data frame
#' @param method A string that states which distance method the user wants to use
#'
#' @import ggdendro
#'
#' @return A list with clustering path
#'
#' @export
#'
hier_clust <- function(data, method = "euclidean") {
  
  if (!all(sapply(data, is.numeric))) {
    stop("Error: Non-numeric variable detected in 'dat' parameter.")
  }
  
  avail_methods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  
  if(!(method %in% avail_methods)) {
    stop("Please put your distance calculation method in all lower case")
  }
  
  n <- nrow(data)
  
  iterations <- 0 #will count the height of the dendrogram by adding 1 to each
  distances <- as.matrix(dist(data, method = method, diag = TRUE, upper = TRUE)) #distance matrix
  
  clusters <- list() #the list from which the clusters will be stored and manipulated
  clusters[1:n] <- lapply(1:n, function(i) i)
  
  while(length(clusters) > 1) { #keep iterating until there is only one cluster left
    
    #browser()
    #print(distances)
    #print(which(distances == min(distances) & row(distances) != col(distances), arr.ind = TRUE))
    #temp_matrix <- distances[row(distances) != col(distances), ]
    
    # print(temp_matrix)
    
    min_index <- which(distances == min(distances) & row(distances) != col(distances), arr.ind = TRUE)
    
    #there are still clusters available for analysis
    if(nrow(min_index) != 0) {
      
      merge_index <- min_index[1, ]
      
      #Hierarchical Clustering Example
      
      #create new cluster that adds in the min dist cluster pairing
      merged_cluster <- c(clusters[[merge_index[1]]], clusters[[merge_index[2]]])
      
      #deletes the indexes of both clusters in the pair and puts the new cluster back into the clusters list
      clusters <- clusters[-merge_index]
      clusters <- c(clusters, list(merged_cluster))
      
      #Average Linkage
      
      #temporary vector that will add a row/column at the end of the matrix that holds the new average-linkage values
      temp_vec <- c()
      
      for(i in 1:nrow(distances)) {
        
        avg_dist <- sum(distances[merge_index[1], i], distances[merge_index[2], i]) / 2
        temp_vec <- c(temp_vec, avg_dist)
      }
      
      #adding in new average linkage column and row (indicating the union of the new cluster)
      distances <- rbind(distances, temp_vec)
      temp_vec <- c(temp_vec, 0) #bc new row was added above, the 0 indicates new distance from itself
      distances <- cbind(distances, temp_vec)
      
      #removing old clusters
      distances <- distances[-merge_index[1], -merge_index[1]]
      distances <- distances[-merge_index[2], -merge_index[2]]
    }
    
  }
  
  iterations <- iterations + 1
  
  return(clusters[[1]])
  
}