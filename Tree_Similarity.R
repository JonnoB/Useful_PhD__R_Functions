Tree_Similarity <- function(tree1, tree2, min_clusters = 1, max_clusters = 20, node_jaccard = FALSE){
  #Comapres two hierachical clustering trees made from the same network.
  #It returns a dataframe of the mean, median and sd or the nodal jaccard similarity.
  #or if node_jaccard is true the full jaccard scores for each node
  #tree2 a an hclust object from a network
  #tree2 an hclust object which contains the same node names. although the actual network structure is different
  #min_clusters the minimum number of clusters to be used
  #max_clusters the maximum number of clusters to be used
  #node_jaccard a logical value, FALSE provides a summary, TRUE provides jaccard scores for all nodes.
  min_clusters:max_clusters %>% map_df(~{
    Comms1 <- tree1 %>%
      list(names = names(cutree(., .x)),  membership = cutree(., .x)) %>%
      MakeNodeCommunity(large = 1, medium = 0)
    
    Comms2 <- tree2 %>%
      list(names = names(cutree(., .x)),  membership = cutree(., .x)) %>%
      MakeNodeCommunity(large = 1, medium = 0)
    
    all_results <-IsSameCommunity2(Comms1, Comms2) 
    
    if(!node_jaccard){
      all_results <- all_results %>%
        summarise(mean = mean(Jaccard), median = median(Jaccard), sd = sd(Jaccard))
      
    }
    
    return(all_results %>%
             mutate(clusters = .x))
    
  })
}