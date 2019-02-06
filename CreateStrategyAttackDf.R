CreateStrategyAttackDf <- function(g){
  #This function takes a graph and creates a dataframe where each column represents the attack order
  #using various different dataframes.
  #Maybe this should be put in the PowerGridNetworking Folder?
  
  AttackOrderNames <- function(g, .fun, ...){
    #Given a function that outputs a node score, this function returns the names of the nodes in descending order,
    NodeDf <- as_data_frame(g, what = "vertices") %>% 
      as_tibble %>%
      mutate(metric = .fun(g, ...)) %>%
      arrange(desc(metric)) %>% .$name
    
    return(NodeDf)
    
  }
  
  ElecCen1<- ElectricalCentrality(g)
  
   StratAttackVect <-  c(AttackOrderNames(g, degree),
                         AttackOrderNames(g, EntropicDegree, Scale = TRUE),
                         AttackOrderNames(g, EntropicDegree,  value = "PowerFlow", Scale = TRUE),
                         ElecCen1$NodeEC %>% arrange(-NodeEC) %>% .$Bus.Name,
                         AttackOrderNames(g, betweenness)) %>% 
   matrix(., nrow = 5, byrow = T) %>%
   as_tibble %>%
   mutate(SimulationID = paste0("Simulation_ID_",1:5)) %>%
   select(SimulationID, everything()) %>%
   setNames(c("SimulationID", paste0("Target", 1:vcount(g))))

  return(StratAttackVect)
  
}