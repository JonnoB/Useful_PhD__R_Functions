Permute_IEEE <- function(g, random_seed){
  #This function permutes the node labels of IEEE networks. It is designed to work on IEEE 118
  #But probably also works for the other networks
  # g I graph network of IEEE power grid
  # random_seed the random seed to be used for sampling
  
  edge_df <- as_data_frame(g)
  
  node_df <- as_data_frame(g, what = "vertices")
  
  temp_gen <-node_df %>%
    filter(Net_Generation > 0)
  
  temp_dem <-node_df %>%
    filter(Net_Generation < 0)
  
  set.seed(random_seed)
  gen_order <- sample(1:nrow(temp_gen), nrow(temp_gen))
  dem_order <- sample(1:nrow(temp_dem), nrow(temp_dem))
  
  temp_gen2 <- temp_gen %>%
    mutate(name = name[gen_order],
           Name = Name[gen_order])
  
  temp_dem2 <- temp_dem %>%
    mutate(name = name[dem_order],
           Name = Name[dem_order])
  
  #Combine all the nodes back together
  temp_nodes <- bind_rows(temp_gen2, temp_dem2, node_df %>%
                            filter(Net_Generation == 0))
  
  #The power flow is calculated just becuase
  g_out  <- graph_from_data_frame(edge_df, directed = FALSE, vertices = temp_nodes)
  
  return(g_out)
  
}
