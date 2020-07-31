#' Return full co-failure list
#' 
#' Large numbers of random attacks often need to be split into separate jobs on the HPC. This function combines
#' all the partial co-failure matrices into a single one.
#' @param folder A character string. The full path to the folder that contains the job files.
#' @param pattern A character string. The file name pattern that defines the co-failures. defaults to 'co_failure'
#' 
#' @export
return_full_co_failure_list <- function(folder, pattern = "co_failure"){

full_mat_list <- vector("list", length = 4)
names(full_mat_list) <- c("node_co_failure" , "node_co_failure_no_target", "edge_co_failure", "edge_co_failure_no_target")
target_files <-list.files(folder, 
                          pattern = pattern, full.names = TRUE)
for(i in 1:length(target_files)){
  temp <- readRDS(target_files[i])
  
  if(i ==1){
    #If it is the first element of the loop simply use the matrices
    full_mat_list$node_co_failure <- temp$node_co_failure
    full_mat_list$node_co_failure_no_target <- temp$node_co_failure_no_target
    full_mat_list$edge_co_failure <- temp$edge_co_failure
    full_mat_list$edge_co_failure_no_target <- temp$edge_co_failure_no_target
    
  } else{
    #If it is not the first element of the loop add in the additional matrices
    full_mat_list$node_co_failure <- full_mat_list$node_co_failure + temp$node_co_failure
    full_mat_list$node_co_failure_no_target <- full_mat_list$node_co_failure_no_target + temp$node_co_failure_no_target
    full_mat_list$edge_co_failure <- full_mat_list$edge_co_failure + temp$edge_co_failure
    full_mat_list$edge_co_failure_no_target <- full_mat_list$edge_co_failure_no_target + temp$edge_co_failure_no_target
    
  }
  
}

return(full_mat_list)

}