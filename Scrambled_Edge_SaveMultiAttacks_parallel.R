Scrambled_Edge_SaveMultiAttacks_parallel <- function(g, target_orders, DeleteOrders , Target, cores = 4){
  #the Foreach parallel version, all the necessary packages are imported
  #This function generates a set of attack folders by permuting the edge excess capacity for a specific graph,
  #where each attack folder contains a series of random attacks with a set edge permutation
  
  # g and igraph object of IEEE118
  #target_orders: the dataframe giving the seed used to generate the scramble order
  #DeleteOrders: the order in which the nodes should be deleted
  # Target whether the "Nodes" or "Edges" will be attacked
  
  registerDoParallel(cores=cores)

  foreach(n = 1:nrow(target_orders), .packages = c("PowerGridNetworking", "rlang", "dplyr", "igraph", "stringr", "purrr", "tidyr")) %dopar% {
    
    Iter <- target_orders %>%
      slice(n)
    
    #Proportionally load the network
    g2 <- Proportional_Load(g, alpha = Iter$ec)
    #scramble the excess capaacity
    edge_order_df <- Create_scrambled_edges(g2, Iter$seed, fract = Iter$fract)
    
    current_graph<- g2 %>%
      set.edge.attribute(., "Link.Limit", value = edge_order_df$Link.Limit)
    
    folder <- Iter$folder_path
    #create folder if it doesn't already exist
    if(!file.exists(folder)){
      dir.create(folder, recursive = T)
    }
    
    #start the multi attack for this particular set up 
    SaveMultiAttacks(current_graph, DeleteOrders, folder, 
                     TotalAttackRounds = 1000, 
                     CascadeMode = TRUE,
                     Demand = "Load_MW",
                     Generation = "Generation_MW",
                     EdgeName = "Link", 
                     VertexName = "name", 
                     Net_generation = "Net_Generation",
                     Target = Target)
    
    print(paste("Saved to", folder))
  }
  
  stopImplicitCluster()
  
}