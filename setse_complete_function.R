#' SETSe complete
#' 
#' does a setse for each node in the graph. The number of iterations are twice the number of nodes in the network
#' @param g an igraph object
#' @param max_iter integer. The maximum number of iterations before stopping. In theory can be set to the diameter +1 although
#' number of nodes is safer
#' 
#' @export

setse_complete_function <- function(g, max_iter = vcount(g), name_attr = "label"){
  
  
  full_list <- vertex_attr(g,name_attr) %>%
    map(~{
      print(.x)
      node_id <- .x
      temp_g <- iso_complete_spring_prep(g, target_name = .x)
      
      temp <- SETSe(temp_g, 
                    force ="force",
                    flow = "flow",
                    distance = "distance",
                    capacity = "edge_capacity",
                    edge_name = "edge_name",
                    k ="k",
                    tstep =0.01/vcount(g), 
                    mass = mean(abs(vertex_attr(temp_g, "force"))), 
                    max_iter = max_iter, 
                    coef_drag = 1, 
                    tol = sum(abs(vertex_attr(temp_g, "force")))/10000,
                    sparse = FALSE,
                    two_node_solution = FALSE,
                    include_edges = TRUE,
                    sample = 1)
      
      
      
      
      temp %>% map(~{ .x %>% mutate(node_id = node_id)})
      
    })
  
  #extract only the elevation data from the embedding
  Out <- 1:length(full_list) %>% map(~{
    full_list[[.x]]$node_embeddings %>%
      select(elevation) %>%
      arrange(-elevation)
  }
  ) %>%
    bind_cols %>%
    as.matrix()
  
  
  return(Out)
  
}
