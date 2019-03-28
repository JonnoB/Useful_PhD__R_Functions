Find_network_balance <- function(g, tstep = 0.5, mass = 2000, maxIter =2000, k = 1000, frctmultiplier = 1, tol = 1e-10, verbose = TRUE){
  
  A <- as_data_frame(g) %>% 
    select(Link, from, to) %>% 
    gather(key = type, Node, -Link) %>%
    arrange(Node) %>%
    mutate(value = ifelse(type =="from", 1, -1)) %>%
    ungroup %>%
    select(-type) %>%
    spread(key = Node, value, fill = 0) %>%
    arrange(Link)
  
  rowdat <- A$Link
  
  A <- A %>% select(-Link) %>%
    as.matrix()
  
  rownames(rowdat)
  
  rm(rowdat)
  
  
  NodeStatus <- as_data_frame(g, what = "vertices") %>%
    select(node = name, force = BalencedPower ) %>%
    mutate(
      z = 0,
      mass = mass,
      NetTension = 0, velocity = 0, 
      friction = 0,
      NetForce = force + NetTension - friction,
      acceleration = NetForce/mass,
      t = 0) #%>%
    #filter(!(node %in% deletenames)) #what is this ish?
  
  Link <- as_data_frame(g)  %>%
    mutate(EdgeName = Link, distance = 1/Y, alpha = Link.Limit/abs(PowerFlow),  k= k*(1-1/alpha)) %>% #arbitary k!
    select(EdgeName, distance, k) 
  
  test <-FindStabilSystem2(NodeStatus, A, Link$k, Link$distance, tstep, maxIter, frctmultiplier, tol, verbose = verbose) 
  
  return(test)
  
}
