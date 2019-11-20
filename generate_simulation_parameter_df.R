#' Generate Simulation Parameter dataframe
#' 
#' This file creates the data frame that the HPC script uses for simulation parameters
#' 
#' @param edge_scramble_keys_path A character string. The path name to the folder that contains all the edge scramble keys.
#' @param power_grid_graphs_path A character string. The folder the graphs/networks are saved to.
#' @param collapse_sets A character string. The folder where the collapsed networks are saved to.
#' @param collapse_set_summaries_path A character string. The folder where the summary dataframes of the collapsed networks are saved to.
#' @param embeddings_path A character string. The folder where the final embeddings are saved to.
#' @param replace_target_order A dataframe. If the files from edge_scramble_keys_path are not going to be used
#' becuase the simulations will be proportionally loaded then the data frame here contains the basic parameters. Otherwise NULL
#' and the files are loaded.
#' @seealso \code{\link{save_params_for_HPC}}
#' @export

generate_simulation_parameter_df <- function(edge_scramble_keys_path, 
                                             power_grid_graphs_path,
                                             collapse_sets,
                                             collapse_set_summaries_path,
                                             embeddings_path,
                                             replace_target_order = NULL){
  
  #The target orders need to be combined into a single dataframe so that each job in the array has it's parameters defined
  parameter_df <-  list.files(edge_scramble_keys_path, full.names = T, pattern = ".rds") %>% 
    map_df(~{
      target_order <-.x
      #expand grid creates a two column tibble which is not what I want, so I have to do this two stage process
      
      #this allows the scrambled and PL parameters to be made in the same function
      if(is.null(replace_target_order)){
        
        temp <- readRDS(target_order)
        
      } else{
        
        temp <- replace_target_order
        
      }
      
      
      temp <- temp %>% as.data.frame() %>%
        expand_grid(df = ., simulation_id = 1:100) 
      
      temp2 <- temp$df %>% mutate(simulation_id = temp$simulation_id, 
                                  permutation = basename(.x) %>% gsub(".rds", "",.))
      return(temp2)
    }) %>% mutate(deletion_seed = simulation_id, #The random seed is the simulation id, this means that each simulation id will 
                  #have the same attack order. This is becuase we are looking at the effect of alpha on collapse not at 
                  #the effect of deletion order
                  scramble_network = ifelse(fract==0, FALSE, TRUE)) 
  
  
  parameter_df <- parameter_df %>%
    left_join(
      tibble(graph_path = list.files(power_grid_graphs_path, full.names = T, pattern = ".rds") %>% {
        file.path(dirname(.) %>% basename(),
                  basename(.))}, 
        permutation = basename(graph_path) %>% 
          gsub(".rds", "",.)), 
      by = "permutation" 
    ) %>%
    mutate(
      sub_path = file.path( #The collapse set and the collapse set summary paths are identiacal apart from the root.
        permutation, 
        paste0("fract_", fract, "_ec_", ec, "_v_", v), 
        paste0("simulation_id_", simulation_id, ".rds" )),
      collapse_path = file.path(basename(collapse_sets), sub_path),
      collapse_summary_path = file.path(basename(collapse_set_summaries_path), sub_path),
      embeddings_path = file.path(basename(embeddings_path), permutation, paste0("fract_", fract, "_ec_", ec, "_v_", v, ".rds") )
    ) %>%
    ungroup %>%
    arrange(simulation_id) %>%
    group_by(simulation_id, ec, permutation) %>% #break the parameter table into sets of twelve where each set contains 
    #every ec level. This allows for a longer and more stable clock time
    mutate(compute_group = 1:n()) %>%
    ungroup %>%
    group_by(simulation_id,  permutation) %>%
    mutate(compute_group_strain = 1:n()) %>% #The strain groups need to be different as there are only 480 of them
    ungroup
  
}