Create_scrambled_edges <- function(g, random_seed, fract){
  #This is a wrapper function to ensure that the random edges scramble code is consistant
  #g the electrical graph which will have it's edges scrambled
  #random_seed the random seed code that will be used for the scramble
  set.seed(random_seed)
  
  Out <- as_data_frame(g) %>%
    mutate(ec = Link.Limit-abs(PowerFlow), #get excess capacity
           ec2 = sub_scramble_vector(ec, fract), #change order of excess capacity
           alpha = (abs(PowerFlow)+ec2)/abs(PowerFlow), #calculate alpha
           Link.Limit = abs(PowerFlow)+ec2)#re-order link limit
  
  return(Out)
  
}