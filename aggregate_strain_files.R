#' aggregate_strain_files
#'
#'This function aggregates a folder full of strain files extracting the useful infodrmation and aggregating into a single dataframe
#'
#' @param folder_name A character string. The file path of the folder containing the the strain RDS files from the SETS_embedding function


aggregate_strain_files <- function(folder_name){
  
  Out <- list.files(folder_name,
                     full.names = T) %>%
    map_df(~{
      
      elements <- basename(.x) %>% str_remove(pattern = ".rds") %>% str_split(pattern = "_")
      
      data_df <- read_rds(.x)

      #dynamics<- data_df$network_dynamics
      node_embeddings <-  data_df$node_embeddings
      
      edge_embeddings <-data_df$edge_embeddings  %>%
        mutate(energy = 0.5*k*strain^2) %>%#added in energy might not include it in plot though..
        summarise(mean_strain = mean(strain),
                  median_strain = median(strain),
                  mean_tension = mean(tension),
                  median_tension = median(tension),
                  energy = sum(energy)) %>%
        mutate(
          iter_max = max(node_embeddings$Iter),
          static_force =   sum(abs(node_embeddings$static_force)),
          fract = elements[[1]][2],
          carrying_capacity = elements[[1]][4],
          largest = elements[[1]][6],
          smallest = elements[[1]][8],
          robin_hood_mode = elements[[1]][11],
          r = elements[[1]][13],
          c = elements[[1]][15],
          tstep = elements[[1]][17],
          drag = elements[[1]][20])
      
      return(edge_embeddings)
      
    }) %>%
    mutate(
      fract = as.numeric(fract),
      carrying_capacity = as.numeric(carrying_capacity),
      largest = as.numeric(largest),
      smallest = as.numeric(smallest),
      robin_hood_mode = as.logical(robin_hood_mode),
      r = as.integer(r),
      c = as.integer(c),
      tstep = as.numeric(tstep),
      drag = as.numeric(drag),
      converged = (static_force < 2e-3)*1) #this needs to be checked when FALSE is present
  
  return(Out)
  
}