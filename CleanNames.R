CleanNames <- function(df, TargetColumn, AlphaOrder){
  #Cleans the names of the simulations so that alphas and real line limits show nicely in plots
  temp <- df  %>%
    mutate_(temp1 = TargetColumn) %>%
    mutate(temp2 = temp1,
           temp2 = temp1 %>%
             str_replace_all(., "_", " ") %>%
             str_extract_all(., "([0-9])+")%>% as.numeric(.)/100,
           temp3 = temp1 %>%
             str_replace_all(., "_", " ") %>%
             str_extract_all(., "([aA-zZ\\s])+", T) %>%
             paste0(., ifelse(is.na(temp2), "", temp2)),
           temp3 = temp3 %>% as.factor() %>%
             fct_relevel(., AlphaOrder)
    ) %>%
    select(temp3)
  temp <-temp  %>% set_names(paste0(TargetColumn,"2")) 
  
  temp <- bind_cols(df, temp)
  return(temp)
}