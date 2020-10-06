#' Clean the embeddings ready for performance evaluation
#' 
#' This is a helper function that is not meant to be generalised beyond this use case
#' 
#' @param folder_path A string. The path the folder containing the facebook graphs
#' @param file_name A string. The name of the uni file to be loaded. this is passed into the function as part of the loop it sits in
#' @param data_node_details A dataframe. It contains the node id the class (called taget) and the year of graduation
#' @param active_period A numeric/integer vector. The year or years that are being evaluated
#'  
#' @details This function is used in the paper "The spring bounces back"
#'  
#' @export


clean_embeddings_pre_eval <- function(folder_path, file_name, active_period, data_node_details){
  
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
        left_join(., data_node_details %>% select(node, target, year),  by = "node") %>%
        filter(year == active_period) %>%
        mutate(model = model_name)
      
      return(out)
    }) %>%
    bind_rows( data_node_details %>% select(-euc_tension2) %>% 
                 rename(X1 =mean_tension2, X2 = elevation2) %>%
                 mutate(model = "SETSe"))  %>%
    #remove the other years and the other student types
    filter( year %in% active_period,
            target !=0) 
  
}
