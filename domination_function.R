#' Domination function
#' 
#' Makes a network where the higher the elevation of the nodes the higher the probability that A dominates B
#' 
#'  @param g_dl An igraph network
#'  @param g_mat A weighted adjacency matrix of the graph
#'  @param  verbose A logical. If TRUE the function prints the name of the current focus node

domination_function <- function(g_dl, g_mat, verbose = TRUE){
  
  #make the diagonal of the matrix be the sum of all conflicts won
  test <- g_mat %>% select(-from) %>% as.matrix() %>% -.
  
  #the top or bottom memebers may never lose/win this needs to be accounted for to stop massive values
  non_zero_row <-rowSums(test) !=0
  
  diag(test) <- -rowSums(test)
  test <- as_tibble(test#/apply(test,1, max)
  ) %>%
    mutate(from =g_mat$from) %>%
    select(from, everything())
  
  #create a projection for each node in the network, this will be it relative to all the wins it has had
  dominance_list <-unique(test$from)[non_zero_row] %>% map(~{
    
    if(verbose) {
      print(.x)
      }
    
    vertex_dataframe <- test %>%
      filter(from == .x) %>%
      select(-from) %>%
      pivot_longer(cols = everything(), names_to = "name", values_to = "force")
    
    g_out <- graph_from_data_frame(as_data_frame(g_dl) %>% 
                                     mutate(
                                       edge_name = paste(from, to),
                                       flow = 1,
                                       distance = 1,
                                       capacity = 1,
                                       k = 1000), directed = FALSE, vertices = vertex_dataframe) 
    
    setse_complete <- auto_SETSe(g_out,
                                 force = "force",
                                 flow = "flow",
                                 distance = "distance",
                                 capacity = "capacity",
                                 edge_name = "edge_name",
                                 k = "k",
                                 mass = sum(abs(vertex_attr(g_out, "force")))/(vcount(g_dl)), #weight is proportional to force
                                 tol = sum(abs(vertex_attr(g_out, "force")))/10000,
                                 hyper_max = 50000,
                                 hyper_iters = 1000,
                                 hyper_tol = 0.001,
                                 step_size = 0.1,
                                 verbose = FALSE)
    
    node_embeddings <- setse_complete$node_embeddings %>%
      select(node, elevation) %>%
      mutate(focus_node = .x)
    
    edge_embeddings <- setse_complete$edge_embeddings %>%
      select(edge_name, tension, strain) %>%
      mutate(focus_node = .x)
    
    Out  <- list(node = node_embeddings, edge = edge_embeddings)
    
    return(Out)
  })  %>%
    transpose() %>%
    map(bind_rows)
  
  
  
  dom_ranks_node <-dominance_list$node %>%
    group_by(focus_node) %>%
    mutate(elevation2 =elevation -min(elevation)) %>% #add the smallest number so that all values are positive
    ungroup %>%
    group_by(node) %>%
    summarise(sum = sum(elevation),
              euc = mean(elevation)#sqrt(sum(elevation2^2))
              ) %>%
    left_join(  dominance_list$edge %>%
                  group_by(focus_node) %>%
                  summarise(tension = mean(tension)) %>%
                  rename(node = focus_node), Joining, by = "node") %>%
    mutate(tension = ifelse(is.na(tension), 0, tension))
    #mutate(rank1 = rank(-sum) ,
    #       rank2 = rank(-euc))
  
  # dominance_list$edge %>%
  #   separate(., col = "edge_name", sep=" ", into = c("node_a", "node_b")) %>%
  #   pivot_longer(cols = node_a:node_b, names_to = "dump", values_to = "node" ) %>%
  # select(-dump) %>%
  #   group_by(node) %>%
  #   summarise(tension = mean(tension), 
  #             counts = n())
  #   group_by(focus_node) %>%
  #   summarise(tension = mean(tension)) %>%
  #   rename(node = focus_node)
  
  
  dom_ranks_edge <- dominance_list$edge %>%
    group_by(edge_name) %>%
    summarise(sum_tension = sum(tension),
              euc_tension = sqrt(sum(tension^2))) %>%
    mutate(rank1 = rank(-sum_tension) ,
           rank2 = rank(-euc_tension))
  
  Out <- list(node = dom_ranks_node, edge = dom_ranks_edge)
  
  return(Out)
}
