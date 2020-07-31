#' Process facebook embeddings
#'
#'Makes the facebook 100 data easier to work with
#'
#'This function processes the facobook embeddings so that they are combined into single files for each element of the 
#'output list of setse_bicomp
#'
#'@param source_path a character string. The path where the HPC output of the facebook 100 data is.
#'@param target_oath a chracter string. The path where the final files will be stored
#'
#'@export
#this loads the facebook embeddings then saves each list element as a separate file so as not to clog up the memory
process_facebook_embeddings <- function(source_path, target_path){
  
  if(!dir.exists(target_path)){dir.create(target_path)}
  
if(!(length(list.files(file.path(target_path)))==7)){
  
  file_paths <- list.files(source_path, full.names = T, pattern = ".rds")
  
  facebook_embeddings_data <- 1:length(file_paths) %>%
    map(~{
      
      print(.x)
      file_name <- basename(file_paths)[.x]
      readRDS(file_paths[.x]) %>%
        flatten() %>%
        map(~{
          
          Out <-  .x %>% mutate(file_name = str_remove(file_name, ".rds"))
          
          
          return(Out)
        })
      
    }) %>% transpose() 
  
  
  embeddings_names <-names(facebook_embeddings_data)
  
  embeddings_names %>%
    walk(~{
      facebook_embeddings_data[[.x]] %>%
        bind_rows() %>%
        saveRDS(., file.path(target_path, paste0("facebook_", .x, ".rds")))
      
    })
  
  rm(embeddings_names)
  rm(facebook_embeddings_data)
  rm(file_paths)
}
  
}
