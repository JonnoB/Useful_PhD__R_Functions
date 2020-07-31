


graph_iso_data_function <- function( files_path){
  
  test <- 1:length(file_path) %>%
    map_df(~{
      print(.x)
      g <- read_graph(file = file_path[.x], format = "dimacs", directed = FALSE) 
      g_degree <- degree(g)
      #  g_betweeness <- centralization.betweenness(g)$res
      
      tibble(id = .x, 
             graph = basename(file_path[.x]),
             edges = ecount(g),
             nodes = vcount(g),
             max_degree = max(g_degree),
             min_degree = min(g_degree),
             diameter = diameter(g),
             #     max_betweenness = max(g_betweeness),
             #    min_betweenness = min(g_betweeness)
      ) %>%
        mutate(diff_degree = max_degree- min_degree)
      
      
    })
  
}
