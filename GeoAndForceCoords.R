GeoAndForceCoords <- function(g, NodePos, seed= 2346){
  #This function takes the graph and a data frame of three columns Node (name of the nodes) and Longitude and Latitude.
  #The function outputs a dataframe that allows the plotting of of a nodes by geography or fruchtman and reingold expansion
  
  test <- g %>%
    as_data_frame(.) %>%
    select(from, to, Link, Voltage) %>%
    gather(key = "type", value = "Node", -Voltage,-Link) %>%
    left_join(NodePos)
  
  set.seed(seed)
  BaseCoords <- layout_nicely(g) %>% 
    as_tibble %>% 
    mutate(Node = names(V(g))) %>%
    rename(Longitude = V1,
           Latitude = V2)
  
  CoordLimstest <-test %>%
    select(Longitude:Latitude) %>%
    summarise_all(funs(min, max, mean, sd))
  
  CoordLims <-BaseCoords %>%
    select(Longitude:Latitude) %>%
    summarise_all(funs(min, max, mean, sd))
  
  BaseCoords <- BaseCoords %>%
    mutate(Longitude = (Longitude-CoordLims$Longitude_min)/(CoordLims$Longitude_max-CoordLims$Longitude_min),
           Latitude =  (Latitude-CoordLims$Latitude_min)/(CoordLims$Latitude_max-CoordLims$Latitude_min),
           Longitude = Longitude*(CoordLimstest$Longitude_max-CoordLimstest$Longitude_min)+CoordLimstest$Longitude_min,
           Latitude = Latitude*(CoordLimstest$Latitude_max-CoordLimstest$Latitude_min)+CoordLimstest$Latitude_min)

  
  test2 <- test %>%
    select(-Longitude, -Latitude) %>%
    left_join(BaseCoords, by = "Node") %>%
    mutate(PositionType = "Fruchterman & Reingold ") %>%
    bind_rows(., test %>% mutate(PositionType = "Geo Space"))
  
  return(test2)
  
}