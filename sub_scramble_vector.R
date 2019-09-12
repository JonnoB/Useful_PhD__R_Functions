sub_scramble_vector <- function(vect, fract){
  #This function randomly samples a fixed fraction of a vector, then permutes that sample,
  #it is useful for scrambling only a fraction of the edges of a network, it is used in Create_scrambled_edges function.
  #The function returns a vector where a subsample of the elements have been re-ordered.
  
  #vect: the vector of values to be scrambled
  #fract: The fraction of elements to be scrambled, a numeric value.
  
  #subsample the vector to get the elements that will be scrambled, and arrange them in order
  
  n <- round(length(vect)*fract)
  
  sample_vect <- sample(1:length(vect), n) %>% sort
  
  #Create a new vector that will replace the original
  vect2 <- vect
  #sample the vector. but only at the randomly chosen indices, to get the scrambled values
  new_sub_value <- sample(vect2[sample_vect], length(vect2[sample_vect]))
  #replace the original values with sub sample at the appropriate index
  vect2[sample_vect] <- new_sub_value
  
  return(vect2)
  
}