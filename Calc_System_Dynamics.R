Calc_System_Dynamics <- function(NodeStatus, EdgeNode, kvect, dvect, tstep = 1, frctmultiplier = 1){
  
  NodeStatus2 <- NodeStatus %>%
    mutate(z = distance(z, velocity, acceleration, t0 = t, t1 = t + tstep),
           velocity = velocity(velocity, acceleration, t, t + tstep),
           NetTension = Create_Tension_matrix(EdgeNode, z, kvect, dvect) %>% rowSums(),
           friction = Calc_Damping_matrix(EdgeNode, velocity, kvect, mass) %>% rowMeans()*frctmultiplier, #velocity*10,
           NetForce = force + NetTension - friction,
           acceleration = NetForce/mass,
           t = t + tstep)
  
  paste("acceleration", sum(abs(NodeStatus2$acceleration)), "velocity", sum(abs(NodeStatus2$velocity)))
  
  return(NodeStatus2)
  
}