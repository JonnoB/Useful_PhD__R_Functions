#' one vs all SETSe clustering
#' 
#' This function performs the slow one vs all clustering approach to graphs.
#' It also compares the output to a few different clustering algos
#' The point of it is to show that SETSe has similar performance characteristics
#' 
#' This is a helper function for analysis of SETSe and is not supposed to be used for real data analysis
#' 
#' @param g_out An igraph object
#' @param k_options2 a dataframe
#' 
#' 
#' @export
#' 
one_vs_all_SETSe <- function(g_out, k_options2){
  
#prep the graph for the analysis
    graph_type <- graph_attr(g_out, "type")
  
  g_df <- as_data_frame(g_out, what = "both")
  
  edge_df <- left_join(
    g_df$edges %>% mutate(edge_name = paste(from, to, sep = "_"),
                          from = as.character(from),
                          to = as.character(to)),
    g_df$vertices %>%
      rename(from_class = class, from_sub_class = sub_class),
    by = c("from"="node")) %>%
    left_join(.,
              g_df$vertices %>%
                rename(to_class = class, to_sub_class = sub_class),
              by = c("to"="node")) %>%
    mutate(
      match_type = case_when(
        from_sub_class == to_sub_class ~ "sub",
        from_class == to_class ~ "class",
        TRUE ~ "inter"
      ),
      flow = 1,
      distance = 1,
      capacity = 1
    ) %>%
    left_join(k_options2, by = "match_type")
  
  
  
  setse_params  <-expand_grid(node =1:40, k = 0)
  
  #create a one vs all setse embedding for each of the nodes
  g_list_one_vs_all <-1:nrow(setse_params) %>% map(~{

    g_out <-graph_from_data_frame(edge_df, directed = FALSE,  g_df$vertices %>% select(node, class, sub_class) %>%
                                    mutate(force = ifelse(node == setse_params$node[.x], 1, -1/39))) #Forces sum to 1
    
    #     save_path <- file.path(PLwd, "peel_influence", paste0("graph_ref_", graph_ref, "_k_", setse_params$k[.x], "_node_",
    #                                                           setse_params$node[.x], ".rds") )
    # if(!file.exists(save_path)){    
    setse_complete <- auto_SETSe(g_out,
                                 force = "force",
                                 flow = "flow",
                                 distance = "distance",
                                 capacity = "capacity",
                                 edge_name = "edge_name",
                                 k = paste0("k_", setse_params$k[.x]),
                                 tol = 40/10000,
                                 hyper_max = 50000,
                                 hyper_tol = 0.001,
                                 step_size = 0.1,
                                 verbose = F)
    
    setse_complete$graphsummary <- tibble(
      strain = mean(abs(setse_complete$edge_embeddings$strain)),
      elevation =  mean(sqrt(
        2 * (setse_complete$node_embeddings$elevation ^ 2)
      )),
      elevation_no_node = left_join(setse_complete$node_embeddings, g_df$vertices, by = "node") %>%
        filter(node != setse_params$node[.x]) %>%
        pull(elevation) %>% mean,
      tension = mean(abs(setse_complete$edge_embeddings$tension)),
      residual_force =  mean(abs(
        setse_complete$node_embeddings$static_force
      )),
      node = setse_params$node[.x]
    ) %>%
      mutate(graph_type = graph_type,
             graph_id = graph_ref)
    
    return(setse_complete)
    
    #  saveRDS(setse_complete, save_path)
    #}
    
  })
  

  #get the graph summaries.
  means_df <-  g_list_one_vs_all %>% map_df(~{
    
   .x$graphsummary
    
  }) %>%
    mutate(graph_id = as.character(graph_id),
           node = as.character(node)) %>%
    left_join(g_df$vertices) %>%
    select(node, everything())
  
  
  #get elevation and strain vector
  elev_vectors <- g_list_one_vs_all %>% map(~{

    data_list <- .x
    graph_summary <- data_list$graphsummary
    
    
    data_list$node_embeddings %>% select(node, elevation) %>% setNames(c("node",paste0("node_", graph_summary$node)))
    
  }) %>%
    reduce(left_join, by = "node")
  
  #The graph used for the rest of the clustering algorithms. The only difference is force is not included
  g_out2 <-graph_from_data_frame(edge_df, directed = FALSE,  g_df$vertices %>% select(node, class, sub_class)) 
  
  means_df2 <- means_df %>%
    mutate(clustering_k4 = kmeans(elev_vectors %>% select(-node) , centers = 4)$cluster %>% factor(),
           clustering_louvain = cluster_louvain( g_out2) %>% membership(),
           clustering_fast_greedy =cluster_fast_greedy(
             g_out2,
             weights = NULL,
             merges = T
           ) %>% as.hclust %>%
             cutree(., k = 4),
           clustering_walktrap = cluster_walktrap(
             g_out2,
             weights = NULL,
             merges = T
           ) %>% as.hclust %>%
             cutree(., k = 4))
  
  
  return(means_df2)
  
  
}

