#' Generate concentrator parameter dataframe
#' 
#' This function generates the parameter dataframe used by the HPC concentrator script.
#'
#' @param graph_name A character string. The name of the igraph file that will be loaded do not include ".rds"
#' @param largest A numeric vector. The fraction of the largest flow edges that will be affected
#' @param smallest A numeric vector. The fraction of the smallest flow edges that will be affected
#' @param fraction A numeric vector. The fraction of excess capacity to be moved
#' @param carrying_capacity a numeric vector. The carrying capacity that the network will be loaded to
#' @param robin_hood_mode A binary vector. Will rob from the rich or rob from the poor
#' @param simulation_id An interger vector. The simulations from 1 to n that will be performed
#' @export
#' @examples 
#' test <- generate_concentrator_parameters("IEEE_118_igraph")
generate_concentrator_parameters <- function(graph_name,
                                             largest = seq(0.0, 0.5, 0.1), 
                                             smallest = seq(0.0, 0.5, 0.1), 
                                             fraction = c(1, 0.5, 0.75, 0.25),
                                             carrying_capacity = c(1.005, 1.025, 1.05, 1.1, 1.2, 1.5, 2, 3, 5, 7, 10, 20) , 
                                             robin_hood_mode = c(TRUE, FALSE),
                                             simulation_id = 1:100,
                                             group_names = c("carrying_capacity", "simulation_id")){
  
  param_df <- expand.grid(largest = largest, 
                          smallest = smallest, 
                          fraction = fraction,
                          carrying_capacity = carrying_capacity, 
                          robin_hood_mode = robin_hood_mode,
                          simulation_id = simulation_id) %>%
    as_tibble() %>%
    mutate(
      deletion_seed = 1:n(),
      graph = graph_name, #this just allows the graph to be before the path even though it is calculated after
      graph_path = file.path("power_grid_graphs", paste0(graph, ".rds")),
      parameter_summary = paste0("fract_", fraction, 
                                 "_ec_", carrying_capacity, 
                                 "_largest_", largest, 
                                 "_smallest_", smallest, 
                                 "_robin_hood_", robin_hood_mode),
      #  graph = basename(graph_path) %>% gsub(".rds", "", .), #real graph name added in
      collapse_base = file.path( #The output collapse paths are identiacal apart from the root.
        graph, parameter_summary, 
        paste0("simulation_id_", simulation_id, ".rds" )),
      embeddings_path = file.path("embeddings", graph, paste0(parameter_summary, ".rds")
      )
    ) %>%
    group_by(.dots = group_names) %>%
    mutate(compute_group = 1:n()) %>%
    ungroup %>%
    group_by(simulation_id) %>%
    mutate(compute_group_strain = 1:n()) %>% #The strain groups is different to the attach groups as all 100 simulations have the same strain
    ungroup  %>%
    #There appears to be a machine tolerance issue created at some point during this function
    #It prevents the strain and attack dataframes from merging as the numbers are not identical
    mutate(carrying_capacity = signif(carrying_capacity),
           smallest = signif(smallest),
           largest = signif(largest),
           fraction = signif(fraction))
  
  return(param_df)
  
}
