
ExtractAttackStats_parallel <- function(RootFolder, 
                                        NewfolderPath, 
                                        Generation = "BalencedPower", 
                                        EdgeName = "name", 
                                        PowerFlow = "PowerFlow", 
                                        Link.Limit = "Link.Limit",
                                        cores = detectCores()){
  
  
  
  registerDoParallel(cores=cores)
  
  foreach(n = list.files(RootFolder , full.names = TRUE), 
          .packages = c("PowerGridNetworking", "rlang", "dplyr", "igraph", "stringr", "purrr", "tidyr")) %dopar% {
            
            
            rootfolder <- n
            targetfolder <- basename(n)
            savename <-paste0(targetfolder, ".rds") %>% file.path(NewfolderPath,.)
            print(savename)
            
            #Create directory if needed
            if(!file.exists(NewfolderPath)){
              dir.create(NewfolderPath)
            }
            
            if(!file.exists(savename)){
              
              print(paste("Extracting summary data for", targetfolder))
              
              summarydata <-list.files(rootfolder) %>%
                map_df(~{
                  print(n)
                  read_rds(file.path(rootfolder, n)) %>%
                    ExtractNetworkStats(Generation = Generation, EdgeName = EdgeName, PowerFlow = PowerFlow, Link.Limit = Link.Limit)%>%
                    mutate( simulationID = gsub("\\.rds", "", n ) %>% gsub("Simulation_ID_", "", .) %>% as.integer)
                }
                ) %>%
                mutate(alpha = targetfolder,
                       GridLoading = ifelse(Blackout==1, 0, GridLoading))
              
              
              saveRDS(summarydata, savename)
              
              print(paste("File", savename, "saved"))
              
            } else {print(paste("saved file for" , targetfolder, "exists"))}
            
          }
  stopImplicitCluster()
  
}
