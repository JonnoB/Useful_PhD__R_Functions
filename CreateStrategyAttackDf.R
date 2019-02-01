CreateStrategyAttackDf <- function(g){
  #This function takes a graph and creates a dataframe where each column represents the attack order
  #using various different dataframes.
  #Maybe this should be put in the PowerGridNetworking Folder?
  
  ElecCen1<- ElectricalCentrality(g)
  #degree
  StratAttackVect <- c(as_data_frame(g, what = "vertices") %>% 
                         as_tibble %>%
                         mutate(metric = degree(g)) %>%
                         arrange(desc(metric)) %>% .$name,
                       #entropic degree line limit
                       as_data_frame(g, what = "vertices") %>%
                         mutate(metric = EntropicDegree(g, Scale = TRUE)) %>%
                         arrange(desc(metric)) %>% .$name,
                       #entropic degree powerflow
                       as_data_frame(g, what = "vertices") %>%
                         mutate(metric = EntropicDegree(g, value = "PowerFlow", Scale = TRUE)) %>%
                         arrange(desc(metric)) %>% .$name,
                       
                       #Electrical centrality
                       ElecCen1$NodeEC %>% arrange(-NodeEC) %>% .$Bus.Name,
                       
                       #centrality
                       as_data_frame(g, what = "vertices") %>%
                         mutate(metric = betweenness(g)) %>%
                         arrange(desc(metric)) %>% .$name
  ) %>% matrix(., nrow = 5, byrow = T) %>%
    as_tibble %>%
    mutate(SimulationID = paste0("Simulation_ID_",1:5)) %>%
    select(SimulationID, everything()) %>%
    setNames(c("SimulationID", paste0("Target", 1:vcount(g))))
  
  return(StratAttackVect)
  
}