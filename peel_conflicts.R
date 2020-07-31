#' peel conflicts
#' 
#' A function that simulates conflicts between nodes on a peels network
#' 
#' @param graph_types a character vector. The peels networks to be used can includes capital letters from A to E
#' @param beligerents a dataframe. A two column data frame containing the node index of each beligerent. The columns must be
#' names "node1" and "node2".
#' @param k_levels a numeric vector. The values the the spring strength will take for each spring type, see details
#' @param  samples an integer. The total number of random graphs to be tested. the seed is set to be the iteration number
#' for reproducibility
#' 
#' @details 
#' The function simulates a conflict for every combination of beligerent pair and graph type for the given k values.
#' 
#' 
#' The k_levels parameters describes the spring strength on each of the three types of link.
#' The first value corresponds to edges which join nodes of the same sub class. The second value corresponds
#' to edges that join nodes of the same class but not sub class. The third value is for edges that join nodes
#' of different sub-classes.
#' 
#' @return a dataframe with the conflict results and associated analysis





peel_conflicts <- function(graph_types = LETTERS[1:5],
                           beligerents = tibble(node1 = 1:2, node2 = 40:39),
                           k_levels = c(1000,500,100),
                           samples = 30){

  parameter_df <- expand_grid(tibble(graph_type = graph_types) , #letters to test
                                   beligerents #nodes to test
)
  
  peels_results <- 1:nrow(parameter_df) %>%
    map_df(~{
      print(.x)
      
      graph_type <- parameter_df$graph_type[.x]
      node_1 <- parameter_df$node1[.x]
      node_2 <- parameter_df$node2[.x]
      
      all_samples <- 1:samples %>%
        map_df(~{
          #set the seed
          set.seed(.x)
          #create Peel graphs
          
          #This loop ensures the peel graph is a single component.
          #It is rare that they are not but it does happen
          nodes <- 0
          while(nodes !=40){
            g <- generate_peels_network(type = graph_type ) %>%
              remove_small_components()
            nodes <- vcount(g)
          }
          #prepare for embedding
          g <- g %>%
            peels_class_k(., k_levels = k_levels) %>% #assign the spring stiffnesses to the edge types
            two_beligerent_conflict(., beligerents = c(node_1, node_2)) %>%# set the force using the beligerents
            prepare_SETSe_continuous(
              node_names = "name",
              force_var = "force") 
          
     
          
          #embed the graph
          result <- g %>%
            auto_SETSe()
          
          
          
          #get the betweeness of each node and the cluster that each node is on
          results_with_cluster <- result$node_embeddings %>%
            mutate(
              clustering_elev =case_when(elevation>0~1,
                                         elevation<0~2,
                                         TRUE~0),#ifelse(elevation>0, 1,2),
              betweenness = betweenness(g, directed = FALSE, weights = get.edge.attribute(g, name = "k"), normalized = F),
              class = get.vertex.attribute(g, "class"),
              sub_class = get.vertex.attribute(g, "sub_class")
            ) %>%
            left_join(.,       tibble(
              node = vertex_attr(g, "name"),
              clustering_fast_greedy = cluster_fast_greedy(
                g ,
                weights = edge_attr(g, "k"),
                merges = T
              ) %>% as.hclust %>%
                cutree(., k = 2),
              clustering_walktrap = cluster_walktrap(
                g ,
                weights = edge_attr(g , "k"),
                merges = T
              ) %>% as.hclust %>%
                cutree(., k = 2)
            ), by = "node"
            )
          
          
          #sumamrise the cluster data in terms of who won
          {
            fract_test <- results_with_cluster %>%
              select(contains("clustering")) %>%
              pivot_longer(cols = contains("clustering")) %>%
              group_by(name, value) %>%
              filter(value !=0) %>%
              summarise(counts = n()) %>%
              ungroup %>%
              group_by(name) %>%
              mutate(fract = counts/sum(counts))
            
            
            fract_test <- results_with_cluster %>% select(contains("clustering")) %>% names() %>%
              map_df( ~ {
                cluster_vect <- results_with_cluster %>% select(all_of(.x)) %>% pull(1)
                
                #if the beligerents are seperated they are in two different clusters
                beligerents_separated <- results_with_cluster %>% select(force , all_of(.x)) %>%
                  filter(force != 0) %>% pull(2) %>%
                  unique(.) %>% length %>% {
                    . > 1
                  }
                
                
                fract_test %>%
                  filter(name == .x) %>% 
                  mutate(separate_success = beligerents_separated)
                
              })
            
            }
          
          
          #combine all the previous data into a single row, that can be stack for each simulation set
          conflict_analysis <- results_with_cluster %>%
            filter(force==1) %>%
            select(class, sub_class, contains("clustering")) %>%
            pivot_longer(cols = contains("clustering"), names_to = "cluster_type") %>%
            left_join(fract_test, by = c("cluster_type"="name", "value") ) %>% select(-value, -counts) %>%
            pivot_longer(cols = c("fract", "separate_success")) %>%
            mutate(name = paste(cluster_type, name, sep = "_")) %>%
            select(-cluster_type) %>%
            pivot_wider(names_from = name, values_from = value)%>%
            mutate(
              betweenness_ratio = results_with_cluster$betweenness[which(results_with_cluster$force==1)]/results_with_cluster$betweenness[which(results_with_cluster$force==-1)],
              class2 = results_with_cluster$class[which(results_with_cluster$force==-1)],
              sub_class2 = results_with_cluster$sub_class[which(results_with_cluster$force==-1)],
              sample = .x,
              node1 = node_1,
              node2 = node_2)
          
          return(conflict_analysis)
          
        }) %>% mutate(graph_type = graph_type)
      
      return(all_samples )
      
    })
  
  return(peels_results)

}
