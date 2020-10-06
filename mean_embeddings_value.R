#' find mean embedding position
#' 
#' This is a helper function that is not meant to be generalised beyond this use case
#' 
#' @param folder_path A string. The path the folder containing the facebook graphs
#' @param file_name A string. The name of the uni file to be loaded. this is passed into the function as part of the loop it sits in
#'  
#' @details This function is used in the paper "The spring bounces back"
#' 
#' @export

mean_embeddings_value <- function(folder_path, file_name){
  
  #g is loaded twice once in the `create_network_vote_performance` and once here. This is inefficient
  #but easy to code. I can change the function if it becomes necessary
  g <- readRDS(list.files(folder_path, full.names = T, pattern = file_name)) %>%
    remove_small_components()
  
  node_name_df <- as_data_frame(g, what = "vertices") %>%
    select(node = name)
  
  #The paths for the python embeddings
  embeddings_paths <- list.files("/home/jonno/setse_1_data/facebookpython", pattern = file_name, full.names = T) %>%
    c(., list.files("/home/jonno/setse_1_data/facebooknode2vec", pattern = file_name, full.names = T))
  
  embeddings <- embeddings_paths %>%
    map_df(~{
      current_path <-.x
      model_name <- str_split(basename(current_path), "_")[[1]][1]
      
      #node2vec has a different file structure to the GEM embeddings so must be processed differently
      if(model_name == "node2vec"){
        
        temp <- read_delim(current_path, 
                           delim = " ",
                           col_names = FALSE,
                           skip = 1) %>% 
          mutate(X1 = str_remove(X1, "n") %>% as.integer()) %>%
          arrange(X1) %>%
          select(X1 = X2, X2 = X3) %>%
          as_tibble
        
      } else {
        
        temp <- read_csv(current_path, col_names = FALSE) 
        
      }
      
      #load the embeddings and bind the data columns. The embeddings are in the same order as the network
      #so this is ok
      #the data node details can then be joined on as the names are known
      out <- temp %>%
        bind_cols(node_name_df, .) %>%
        mutate(model = model_name)
      
      return(out)
    }) %>%
    group_by(model) %>%
    summarise(mean_X1 = mean(X1),
              mean_X2 = mean(X2),
              mean_abs_X1 = mean(abs(X1)),
              mean_abs_X2 = mean(abs(X2))) %>%
    mutate(file_name = file_name)
  
}
