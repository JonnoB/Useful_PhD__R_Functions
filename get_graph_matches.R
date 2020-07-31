#' find the matches between nodes
#' 
#' 
#' @param network_a a matrix
#' @param network_b a matrix
#' 
#' @export

get_graph_matches <- function(network_a, network_b){
  
  #this should probably be taken out of the function
  cosine_mat <- function(mat_a, mat_b){
    #calculate the dot product of the matrix columns
    dot_prod_mat <- crossprod(mat_a, mat_b)
    #the magnitude of the columns
    mag_mat <- sqrt(colSums(mat_a^2))*sqrt(colSums(mat_b^2))
    dot_prod_mat/mag_mat
    
  }
  
  #perform cosine similarity
  test2 <- cosine_mat(network_a, network_b)
  #check if the differrence between 1 and the cosine similarity is smaller than the machine tolerance
  test3 <- abs(1-(test2)) <.Machine$double.eps^0.5
  
  #get the return pairs that are identical
  #then summarise the counts of each node
  #the highest common factor is the automorphism
  test4 <- which(test3==1, arr.ind=TRUE) %>%
    as_tibble() %>%
    rename(row  = 1, column = 2) %>%
    mutate(node_a = rownames(test3)[row],
           node_b = colnames(test3)[column]) %>%
    pivot_longer(cols = node_a:node_b, names_to = "node_type", values_to = "focus_node") %>%
    group_by(focus_node) %>%
    summarise(counts = n())
  
  return(test4)
  
}