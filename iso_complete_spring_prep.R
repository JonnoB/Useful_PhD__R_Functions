#' iso complete preparation
#' 
#' This function prepares a graph for every node being checked for iso-morphism
#' 
#' @param g an igraph object. 
#' @param target_name The name of the node which will be the focus of the SETSe evaluation. This node gets the positive force
#' 
#' @export

iso_complete_spring_prep <- function(g, target_name){
  
  edge_change <- as_data_frame(g) %>%
    mutate(distance = 1,
           flow = 1,
           edge_capacity = Inf,
           edge_name = paste(from, to, sep ="-"))
  
  #the edges all have the same force so can be normalised on force creation easily.
  #The Links all have effectively infinite capacity so that can't be normalised and just takes the least stretchy value.
  node_change <- as_data_frame(g, what = "vertices") %>%
    mutate(name = 1:n(),
           force = ifelse(name==target_name, 1, -1/(vcount(g)-1))) %>%
    select(name, everything())
  
  current_graph  <- graph_from_data_frame(edge_change, directed = FALSE, vertices = node_change)%>%
    set.edge.attribute(. , "distance", value = 1) %>%
    set.edge.attribute(., "Area", value = 1) %>%
    calc_spring_youngs_modulus(., "flow", "edge_capacity", minimum_value = 100, stretch_range = 1000) %>%
    calc_spring_constant(., E ="E", A = "Area", distance = "distance") 
  
  return(current_graph)
}
