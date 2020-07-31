#' Peels spring prep
#'
#' Preps a network that is part of the peels quintet family to be emmbedded using setse
#'
#' @param  g an igraph object. A network that has been created to contain two classes and have the peels quintet structure
#' @export
#' 
peels_spring_prep <- function(g){
  
  edge_change <- as_data_frame(g) %>%
    mutate(distance = 1,
           flow = 0,
           Link.Limit = Inf,
           Link = paste(from, to, sep ="-"))
  
  #the edges all have the same force so can be normalised on force creation easily.
  #The Links all have effectively infinite capacity so that can't be normalised and just takes the least stretcy value.
  node_change <- as_data_frame(g, what = "vertices") %>%
    mutate(name = 1:n(),
           Generation = ifelse(class=="A", 1/20, 0 ),
           Demand = ifelse(class=="B", 1/20,0),
           Net_Generation = Generation-Demand) %>%
    select(name, everything())
  
  current_graph  <- graph_from_data_frame(edge_change, directed = FALSE, vertices = node_change)%>%
    set.edge.attribute(. , "distance", value = 1) %>%
    set.edge.attribute(., "Area", value = 1) %>%
    calc_spring_youngs_modulus(., "flow", "Link.Limit", minimum_value = 100, stretch_range = 1000) %>%
    calc_spring_constant(., E ="E", A = "Area", distance = "distance") 
  
  return(current_graph)
}
