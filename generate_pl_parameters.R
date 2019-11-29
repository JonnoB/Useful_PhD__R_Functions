#' Generate PL parameter dataframe
#' 
#' This function generates the parameter dataframe used by the HPC PL script.
#'
#' @param A character string. The name of the igraph file that will be loaded do not include ".rds"
#' @export
#' @examples 
#' test <- generate_concentrator_parameters("IEEE_118_igraph")
generate_pl_parameters <- function(graph_name){
  
  param_df <-  expand.grid(
    carrying_capacity = c(1, 1.02, 1.01, 1.005, 1.05, 1.1, 1.2, 1.5, 2, 2.5, 3, 5, 7, 10, 15, 20, 50, 100, 200, Inf),
    simulation_id = 1:100) %>%
    as_tibble() %>%
    mutate(
      deletion_seed = 1:n(),
      graph = graph_name, #this just allows the graph to be before the path even though it is calculated after
      graph_path = file.path("power_grid_graphs", paste0(graph, ".rds")),
      parameter_summary = paste0( "ec_", carrying_capacity),
      collapse_base = file.path( #The output collapse paths are identiacal apart from the root.
        paste0("PL_", graph), parameter_summary, 
        paste0("simulation_id_", simulation_id, ".rds" )),
      embeddings_path = file.path("embeddings", paste0("PL_", graph), paste0(parameter_summary, ".rds")
      )
    ) %>%
    mutate(compute_group = simulation_id) %>%
    ungroup %>%
    mutate(compute_group_strain = 1:n()) 
  
  return(param_df)
  
}
