Scrambled_Edge_SaveMultiAttacks <- function(target_orders, Scrambled_edge_cap, folder, DeleteOrders, Target = "Nodes", fract = 1){
  #This function generates a set of attack folders by permuting the edge excess capacity for a specific graph,
  #where each attack folder contains a series of random attacks with a set edge permutation
  
  #target_orders: the dataframe giving the seed used to generate the scramble order
  #Scrambled_edge_cap: the graph an igraph object
  #folder: The folder root name, will be appended with the iteration number for each row of the target_orders
  #DeleteOrders: the order in which the nodes should be deleted
  1:nrow(target_orders) %>% map(~{
    
    edge_order_df <- Create_scrambled_edges(Scrambled_edge_cap, .x, fract = fract)
    
    current_graph<- Scrambled_edge_cap %>%
      set.edge.attribute(., "Link.Limit", value = edge_order_df$Link.Limit)
    
    folder <- paste0(folder, .x)
    #create folder if it doesn't already exist
    if(!file.exists(folder)){
      dir.create(folder)
    }
    
    SaveMultiAttacks(current_graph, DeleteOrders, folder, 
                     TotalAttackRounds = 1000, 
                     CascadeMode = TRUE,
                     Demand = "Load_MW",
                     Generation = "Generation_MW",
                     EdgeName = "Link", 
                     VertexName = "name", 
                     Net_generation = "Net_Generation",
                     Target = Target)
    
  })
  
}