calcgeodist <- function(points, Cabledist){
  #calculates the distance between all nodes in the network. produces distance for both the cable length from the data and the geodistance using the coordinates. After using this function the error can be found.
  #This is used for correcting errors in the coordinates of the nodes
  
  #The columns used are Lat and Lon
  #select variables
  TempPoints <- points %>%
    select(name, Lat, Lon) 
  
  #Add on cable length
  CableTest <- Cabledist %>%
    left_join(., TempPoints, by = c("Bus.1"= "name")) %>%
    left_join(., TempPoints, by = c("Bus.2"= "name")) 
  
  #Find distance based on coordinates
  test <- 1:nrow(CableTest) %>%
    map_df(~{
      CableTest[.x,] %>%
        mutate(geodist = distm(as.matrix(data_frame(Lon.x, Lat.x)), 
                               as.matrix(data_frame(Lon.y, Lat.y)))/1000) #convert to meters
      
    }) %>%
    mutate(geodist = round(geodist, 2),
           diff = Length - geodist,
           absdiff = abs(diff),
           ratio = absdiff/Length)
}