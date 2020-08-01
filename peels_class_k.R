#' Peels class k
#'
#' adds spring stiffnesses by relationship type to a Peel graph
#' 
#' @param g An igraph object. 
#' @param k_levels The three spring stiffnesses see details
#' 
#' @details 
#' The k_levels parameters describes the spring strength on each of the three types of link.
#' The first value corresponds to edges which join nodes of the same sub class. The second value corresponds
#' to edges that join nodes of the same class but not sub class. The third value is for edges that join nodes
#' of different sub-classes.
#'
#'
#'@export
peels_class_k <- function(g, k_levels = c(1000, 500, 100)){
  
  g_df_vertices <- as_data_frame(g, what = "vertices")
  
  g_df_edges <- as_data_frame(g, what = "edges") %>%
    left_join(g_df_vertices, by = c("from"="name")) %>%
    left_join(g_df_vertices, by = c("to"="name")) %>%
    mutate(class_match = class.x == class.y,
           sub_class_match = sub_class.x == sub_class.y,
           k = case_when(
             class_match & sub_class_match ~ k_levels[1], #sub class match
             class_match & !sub_class_match ~ k_levels[2], #class amtch
             TRUE ~k_levels[3] #no match
             
           )) %>%
    {.[,c(1,2,9)]}
  
  g <- graph_from_data_frame(g_df_edges, directed = FALSE, vertices = g_df_vertices)
  
  return(g)
  
}