ReOrderGraph <- function(g, Sim_Order){
  
  g2 <-g
  
  for(n in c("Demand", "Generation", "BalencedPower")){
    g2 <- set.vertex.attribute(g2, n, 
                               value = get.vertex.attribute(g2, n )[Sim_Order])
  }
  
  return(g2)
}