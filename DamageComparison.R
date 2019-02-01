DamageComparison <- function(df, Target = "Blackout"){
  #compares the damge of each line limit to the real damage using several metrics
  df <- df %>% rename_(Target = Target)
  
  1:100 %>% map_df(~{
    
    SimAttack <- df %>%
      filter(simulationID == .x) %>%
      select(Target, alpha, NodesAttacked) %>% 
      spread(key = alpha, value = Target) %>%
      select(-NodesAttacked) %>%
      filter(Real_Limits !=1)
    
    SimAttack[is.na(SimAttack)] <- 1
    
    test2 <- SimAttack %>% map_df(~{
      c(postResample(.x, SimAttack$Real_Limits) , 
        mean(abs((SimAttack$Real_Limits - .x)/SimAttack$Real_Limits), na.rm = T)
      ) %>%
        matrix %>%
        t %>%
        data.frame() %>% 
        set_names(names(postResample(SimAttack$Real_Limits, .x)), "MAPE" )
    }
    ) %>% mutate(type = names(SimAttack),
                 SimulationID = .x)
    
  }) %>% 
    rename(alpha  = type) %>%
    filter(alpha != "Real_Limits") %>%
    # mutate(alpha = ifelse(alpha== "Infinite_Limits","Inf", alpha),
    #      alpha = gsub("alpha_value_", "", alpha),
    #      alpha = as.numeric(alpha)/100) 
    CleanNames(., "alpha", AlphaOrder) 
}