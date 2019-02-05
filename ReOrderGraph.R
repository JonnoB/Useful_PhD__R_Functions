ReOrderGraph <- function(g, Sim_Order){
  #This function simply re orders the load and demand of a netowork accross nodes.
  #An upgrade to this function would allow for choice over Nodetype to be selected
  
  g2 <-g
  
  for(n in c("Demand", "Generation", "BalencedPower")){
    g2 <- set.vertex.attribute(g2, n, 
                               value = get.vertex.attribute(g2, n )[Sim_Order])
  }
  
  return(g2)
}