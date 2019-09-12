DamageComparison <- function(df, Target = "Blackout", AlphaOrder){
  #compares the damge of each line limit to the real damage using several metrics
  df <- df %>% rename(Target = Target)
  
  1:max(df$simulationID) %>% map_df(~{
    
    #Filter to a single simulation ID and spread so that each alpha is its own column. The Real Line limits are included in the alphas
    SimAttack <- df %>%
      filter(simulationID == .x) %>%
      select(Target, alpha, NodesAttacked) %>% 
      spread(key = alpha, value = Target) %>%
      select(-NodesAttacked) %>%
      filter(Real_Limits !=1)
    
    #Fill any NA values with 1 as NAs are caused by the complete collapse of the grid
    SimAttack[is.na(SimAttack)] <- 1
    
    #Calculate the error against the real line limits
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
   #Once complete repeat for all other values of .x
    
  }) %>% 
    rename(alpha  = type) %>%
    filter(alpha != "Real_Limits") %>%
    # mutate(alpha = ifelse(alpha== "Infinite_Limits","Inf", alpha),
    #      alpha = gsub("alpha_value_", "", alpha),
    #      alpha = as.numeric(alpha)/100) 
    CleanNames(., "alpha", AlphaOrder) 
}
