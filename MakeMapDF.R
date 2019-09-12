MakeMapDF <- function(g, points, seed = 4747){
  
  #This function can be improved by taking the already aggreggated points file. This will make it more flexible
  
  
  #This function takes 2 (+1) inputs and outputs a dataframe of the graph ready for plotting
  #g: the graph of the network
  #points: a dataframe of the substation node coordinates
  #seed: a random seeded used for the force expansion
  
  #split coords
  coords2 <-  str_split_fixed(string = points$Lat.2, pattern = ",", n = 2)
  #place coords in df
  points <- points %>%
    mutate(Lat.2 = coords2[,1] %>% as.numeric,
           Lon.2 = coords2[,2] %>% as.numeric)
  rm(coords2)
  
  #reduce to site level
  pointsTemp <- points %>%
      filter(!grepl("Derby_South_132kV", SubstationName)) %>% #The node is definately not Derby south
    mutate(Lat = ifelse(!is.na(Lat.2), Lat.2, Lat),
           Lon = ifelse(!is.na(Lon.2), Lon.2, Lon),
           name = str_sub(Edges, 1, 4) %>% gsub("-|_", "",.)) %>%
    select(name, Lat, Lon) %>%
    group_by(name) %>%
    summarise_all(funs(mean, diff={max(.)-min(.)})) %>%
    rename(Lat = Lat_mean,
           Lon = Lon_mean)
  
  #merge with all nodes
  pointsTemp <- get.vertex.attribute(g, "name") %>% tibble(name = .) %>%
    left_join(., pointsTemp) %>%
    group_by(name) %>%
    mutate(counts = n()) %>%
    ungroup %>%
    group_by(name) %>%
    mutate(count = n()) %>%
    ungroup
  
  #find nodes with no coords
  NApoints <- pointsTemp %>%
    filter(is.na(Lon)) %>%
    pull(name) %>% str_sub(., 1, 4) %>% gsub("-|_", "",.) %>%  unique()
  
  #make temporary network to find missing coords
  g2 <- set.vertex.attribute(g, "Lat", value = pointsTemp$Lat) %>%
    set.vertex.attribute(., "Lon",value =  pointsTemp$Lon)
  
  #find the average coordinates of the nodes with no coordinates based on thier ego networks
  AllPoints <- NApoints  %>% map_df(~{
    
    pointsTemp %>% 
      filter(name %in% names(ego(g2, nodes = .x)[[1]])) %>%
      summarise(Lon = mean(Lon, na.rm = T),
                Lat = mean(Lat, na.rm = T),
                count = nrow(.), name = .x)
    
  })
  
  #make final graph
  g3 <- set.vertex.attribute(g2, "Lat", index = NApoints, value = AllPoints$Lat) %>%
    set.vertex.attribute(., "Lon",index = NApoints, value = AllPoints$Lon)
  
  Positioning <- matrix(c(get.vertex.attribute(g3, "Lon"),
                          get.vertex.attribute(g3, "Lat")),
                        ncol = 2) %>% as_tibble()  %>%
    rename(Latitude = V2,
           Longitude = V1)
  
  GGmapData <- Positioning  %>%
    mutate(Node = get.vertex.attribute(g3, "name")) %>%
    GeoAndForceCoords(g3,NodePos = ., seed)
  
  return(GGmapData)
  
}
  