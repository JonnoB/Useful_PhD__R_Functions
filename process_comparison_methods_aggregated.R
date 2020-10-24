#' process_comparison_methods_aggregated
#'
#' This is just a wrapper for a bunch of loading data
#' This function is horrible and is only supposed to reduce clutter on the workspace
#' 
#' @export

process_comparison_methods_aggregated <- function(){
  peels_metrics <- metric_set(accuracy, precision, recall, f_meas)
  #Get the GEM embeddings at graph level
  embedded_files <- list.files(file.path(PLwd, "peel_benchmark_embeddings2"), full.names = T)
  
  #calculate the embedding sub-class classification
  include <-!grepl(embedded_files, pattern = "SDNE")
  GEM_embeds <- embedded_files[include] %>%
    map_df(~{
      
      print(.x)
      embedded_path <- .x#embedded_files[50]
      
      #split the file name to get the graph information
      graph_data <- str_split(basename(embedded_path), pattern = "_", simplify = T )
      
      #read the csv in from the folder
      csv_df <- read.csv(embedded_path, header = FALSE) #col_names = FALSE)
      
      #add the embedding values to the matching graph
      g_temp <- multi_quintet[[as.integer(graph_data[6])]]
      for(i in 1:ncol(csv_df)){
        #add in each dimension in the dataset
        g_temp <-  g_temp %>%
          set_vertex_attr(., paste0("dimension_", i), value =  csv_df %>% pull(i))
        
        
      }
      
      #create dataframe to perform linear separation analysis on
      test_df <- as_data_frame(g_temp, what = "vertices") %>%
        mutate(class = factor(class),
               sub_class = factor(sub_class))
      
      #a couple of the methods have twice as many dimensions but half of them are doubles.
      #This means the matrices are not full rank
      #ensureing only the first half of the variables are taken means that the matrices become full rank again
      number_of_columns <-3+as.integer(graph_data[3])
      
      class_formula <- as.formula(paste("class", 
                                        paste(names(test_df)[4:number_of_columns], collapse=" + "), sep=" ~ "))
      sub_class_formula <- as.formula(paste("sub_class", 
                                            paste(names(test_df)[4:number_of_columns], collapse=" + "), sep=" ~ "))
      
      #class_mod <- svm(class ~  dimension_2, test_df, kernel = "linear")
      class_mod <- vglm( class_formula , family=multinomial, data = test_df)
      
      #sub_mod <- svm(sub_class ~  dimension_2, test_df, kernel = "linear")
      sub_mod <- vglm(sub_class_formula, family=multinomial, data = test_df)
      
      class_preds_vect <- predict(class_mod, decision.values = TRUE, type = "response") %>% { colnames(.)[apply(., 1, which.max)]}
      sub_preds_vect <- predict(sub_mod, decision.values = TRUE, type = "response") %>% { colnames(.)[apply(., 1, which.max)]}
      
      test_df <- test_df %>%
        mutate(class_preds =  factor(class_preds_vect, levels = c("A", "B")),
               sub_preds  = factor(sub_preds_vect, levels = c("A_1", "A_2", "B_1", "B_2")))
      
      # test_df <- test_df %>%
      #   mutate(class_preds = predict(class_mod, decision.values = TRUE),
      #          sub_preds  = predict(sub_mod, decision.values = TRUE))
      
      bind_rows(peels_metrics(test_df, truth = class, estimate = class_preds) %>%
                  mutate(model_type = "class"),
                peels_metrics(test_df, truth = sub_class, estimate = sub_preds)%>%
                  mutate(model_type = "sub_class")
      ) %>%
        mutate(embeddings_method = graph_data[1],
               embeddings_dimensions = graph_data[3],
               graph_id = as.integer(graph_data[6]),
               type = str_remove(graph_data[8], pattern = ".csv"))
      
    })
  
  #Calculate the average x y points for the embeddings
  GEM_embeds_agg <- embedded_files %>%
    map_df(~{
      
      print(.x)
      embedded_path <- .x#embedded_files[50]
      
      #split the file name to get the graph information
      graph_data <- str_split(basename(embedded_path), pattern = "_", simplify = T )
      
      #read the csv in from the folder
      csv_df <- read.csv(embedded_path, header = FALSE) #col_names = FALSE)
      
      #add the embedding values to the matching graph
      g_temp <- multi_quintet[[as.integer(graph_data[6])]] %>%
        set_vertex_attr(., "dimension_1", value =  csv_df %>% pull(1)) %>%
        set_vertex_attr(., "dimension_2", value =  csv_df %>% pull(2))
      
      #create dataframe to perform linear separation analysis on
      test_df <- as_data_frame(g_temp, what = "vertices") %>%
        mutate(class = factor(class),
               sub_class = factor(sub_class)) %>%
        mutate(embeddings_method = graph_data[1],
               embeddings_dimensions = graph_data[3],
               graph_id = as.integer(graph_data[6]),
               type = str_remove(graph_data[8], pattern = ".csv"))
      
      
    }) %>%
    group_by(embeddings_method, graph_id, type, embeddings_dimensions) %>%
    summarise(dimension_1 = mean(dimension_1),
              dimension_2 = mean(dimension_2))
  
  
  #Get the SETSe values
  node_data2 <- readRDS(file.path(PLwd, "node_data2.rds"))
  
  #why are there apparently double rows? what is happening with the detected communities dataframe
  SETSe_embeds <- 1:500 %>%
    map_df(~{
      
      test_df <- node_data2 %>%
        filter(graph_id == .x) %>%
        mutate(class = factor(class),
               sub_class = factor(sub_class)) %>%
        rename(dimension_1 = elevation, 
               dimension_2 = tension_mean)
      
      #class_mod <- svm(class ~  dimension_2, test_df, kernel = "linear")
      class_mod <- vglm(class ~ dimension_1 + dimension_2, family=multinomial, data = test_df)
      
      #sub_mod <- svm(sub_class ~  dimension_2, test_df, kernel = "linear")
      sub_mod <- vglm(sub_class ~ dimension_1 + dimension_2, family=multinomial, data = test_df)
      
      class_preds_vect <- predict(class_mod, decision.values = TRUE, type = "response") %>% { colnames(.)[apply(., 1, which.max)]}
      sub_preds_vect <- predict(sub_mod, decision.values = TRUE, type = "response") %>% { colnames(.)[apply(., 1, which.max)]}
      
      test_df <- test_df %>%
        mutate(class_preds =  factor(class_preds_vect, levels = c("A", "B")),
               sub_preds  = factor(sub_preds_vect, levels = c("A_1", "A_2", "B_1", "B_2")))
      
      # test_df <- test_df %>%
      #   mutate(class_preds = predict(class_mod, decision.values = TRUE),
      #          sub_preds  = predict(sub_mod, decision.values = TRUE))
      
      bind_rows(peels_metrics(test_df, truth = class, estimate = class_preds) %>%
                  mutate(model_type = "class"),
                peels_metrics(test_df, truth = sub_class, estimate = sub_preds)%>%
                  mutate(model_type = "sub_class")
      ) %>%
        mutate(embeddings_method = "SETSe",
               embeddings_dimensions = "2",
               graph_id = as.integer(.x),
               type = unique(test_df%>% pull(graph_class)))
      
      
    })
  
  node_data_agg <-node_data2 %>%
    rename(type = graph_class) %>%
    group_by(graph_id, type) %>%
    summarise(dimension_1 = mean(tension_mean), 
              dimension_2 = mean(abs(elevation))) %>%
    ungroup %>%
    mutate(embeddings_method = "SETSe",
           embeddings_dimensions = "2",
           graph_id = as.integer(graph_id))
  
  
  #node2vec data
  node2vec_embeds <- list.files(file.path(PLwd, "node2vec_embeddings"), full.names = TRUE)
  
  node_perf <- node2vec_embeds %>%
    map_df(~{
      
      print(.x)
      embedded_path <- .x#embedded_files[50]
      
      #split the file name to get the graph information
      graph_data <- str_split(basename(embedded_path), pattern = "_", simplify = T )
      
      #read the csv in from the folder
      csv_df <- read.table(embedded_path, header = FALSE, sep = " ", skip = 1) %>%
        mutate(V1 = str_remove(V1, "n") %>% as.integer() %>%{as.character(.+1)})
      
      #add the embedding values to the matching graph
      g_temp <- multi_quintet[[as.integer(graph_data[7])]] #%>%
      # set_vertex_attr(., "dimension_1", value =  csv_df %>% pull(1)) %>%
      # set_vertex_attr(., "dimension_2", value =  csv_df %>% pull(2))
      
      #create dataframe to perform linear separation analysis on
      test_df <- as_data_frame(g_temp, what = "vertices") %>%
        left_join(csv_df, by = c("node"="V1")) %>%
        mutate(class = factor(class),
               sub_class = factor(sub_class))
      
      dimensions_names <-   paste0("dimension_", 1:length(    names(test_df)[-c(1:3)]))
      
      names(test_df)[-c(1:3)] <-   dimensions_names
      
      
      class_formula <- as.formula(paste("class", 
                                        paste(names(test_df)[4:ncol(test_df)], collapse=" + "), sep=" ~ "))
      sub_class_formula <- as.formula(paste("sub_class", 
                                            paste(names(test_df)[4:ncol(test_df)], collapse=" + "), sep=" ~ "))
      
      class_mod <- vglm( class_formula , family=multinomial, data = test_df)
      
      sub_mod <- vglm(sub_class_formula, family=multinomial, data = test_df)
      
      
      class_preds_vect <- predict(class_mod, decision.values = TRUE, type = "response") %>% { colnames(.)[apply(., 1, which.max)]}
      sub_preds_vect <- predict(sub_mod, decision.values = TRUE, type = "response") %>% { colnames(.)[apply(., 1, which.max)]}
      
      test_df <- test_df %>%
        mutate(class_preds =  factor(class_preds_vect, levels = c("A", "B")),
               sub_preds  = factor(sub_preds_vect, levels = c("A_1", "A_2", "B_1", "B_2")))
      
      # test_df <- test_df %>%
      #   mutate(class_preds = predict(class_mod, decision.values = TRUE),
      #          sub_preds  = predict(sub_mod, decision.values = TRUE))
      
      bind_rows(peels_metrics(test_df, truth = class, estimate = class_preds) %>%
                  mutate(model_type = "class"),
                peels_metrics(test_df, truth = sub_class, estimate = sub_preds)%>%
                  mutate(model_type = "sub_class")
      ) %>%
        mutate(embeddings_method = graph_data[1],
               embeddings_dimensions = graph_data[4],
               graph_id = as.integer(graph_data[7]),
               type = str_remove(graph_data[9], pattern = ".csv"))
      
      
    })
  
  n2v_embeds_agg <-  node2vec_embeds %>%
    map_df(~{
      
      print(.x)
      embedded_path <- .x#embedded_files[50]
      
      #split the file name to get the graph information
      graph_data <- str_split(basename(embedded_path), pattern = "_", simplify = T )
      
      #read the csv in from the folder
      csv_df <- read.table(embedded_path, header = FALSE, sep = " ", skip = 1) %>%
        mutate(V1 = str_remove(V1, "n") %>% as.integer() %>%{as.character(.+1)})
      
      #add the embedding values to the matching graph
      g_temp <- multi_quintet[[as.integer(graph_data[7])]] 
      
      #create dataframe to perform linear separation analysis on
      test_df <- as_data_frame(g_temp, what = "vertices") %>%
        left_join(csv_df, by = c("node"="V1")) %>%
        rename(dimension_1 = V2, dimension_2 = V3) %>%
        mutate(class = factor(class),
               sub_class = factor(sub_class))%>%
        mutate(embeddings_method = graph_data[1],
               embeddings_dimensions = graph_data[4],
               graph_id = as.integer(graph_data[7]),
               type = str_remove(graph_data[9], pattern = ".csv"))
      
      
    }) %>%
    group_by(embeddings_method, graph_id, type, embeddings_dimensions) %>%
    summarise(dimension_1 = mean(dimension_1),
              dimension_2 = mean(dimension_2))
  
  
  #Load the stellar DGI data
  
  DGI_embedded_files <- list.files("/home/jonno/setse_1_data/peel_benchmark_stellargraph2", full.names = T)
  
  #This loop has code to remove zero variance columns as these cause a rank deficiency in the classification algo
  #The two dimensions can also be perfectly corellated, this is also corrected
  DGI_embeds <- DGI_embedded_files  %>%
    map_df(~{
      
      print(.x)
      embedded_path <- .x#embedded_files[50]
      
      #split the file name to get the graph information
      graph_data <- str_split(basename(embedded_path), pattern = "_", simplify = T )
      
      #read the csv in from the folder
      csv_df <- read.csv(embedded_path, header = FALSE, skip = 1) #col_names = FALSE)
      zero_var_cols <- nearZeroVar(csv_df ) #identify zero var columns
      csv_df <- csv_df %>% select(-all_of(zero_var_cols)) #remove zero var columns
      
      #in the case that there is more than 1 data column after the 0 variance colunmns have been removed then check for perfect corellation
      if(ncol(csv_df)>2){
        print("checking for correlation")
        #froms stack overflow 
        #https://stackoverflow.com/questions/18275639/remove-highly-correlated-variables
        df2 = cor(csv_df %>%select(-V1))
        hc = findCorrelation(df2, cutoff=0.99) # putt any value as a "cutoff" 
        hc = sort(hc)+1 #add one as there is the name column
        csv_df = csv_df%>% select(-all_of(hc)) #[,-c(hc)]
      }
      
      
      
      #add the embedding values to the matching graph
      g_temp <- multi_quintet[[as.integer(graph_data[6])]]
      
      if(ncol(csv_df)==1){
        
        test_df <- as_data_frame(g_temp, what = "vertices") %>%
          mutate(class = factor(class),
                 sub_class = factor(sub_class)) %>%
          mutate(class_preds = ifelse(class =="A", "B", "A"),
                 sub_preds = ifelse(sub_class =="A_1", "B_1", "A_1"),
                 class_preds =  factor(class_preds, levels = c("A", "B")),
                 sub_preds  = factor(sub_preds, levels = c("A_1", "A_2", "B_1", "B_2"))
          )
        
      } else {
        
        # for(i in 2:ncol(csv_df)){ #The first column from the stellargraph is the node id
        # #add in each dimension in the dataset
        # g_temp <-  g_temp %>%
        #   set_vertex_attr(., paste0("dimension_", i), value =  csv_df %>% pull(i))
        #   
        #   
        # }
        
        number_of_variables_df <-ncol(csv_df)-1
        
        #create dataframe to perform linear separation analysis on
        # test_df <- as_data_frame(g_temp, what = "vertices") %>%
        #   mutate(class = factor(class),
        #          sub_class = factor(sub_class))
        
        test_df <- as_data_frame(g_temp, what = "vertices") %>%
          left_join(csv_df %>%
                      mutate(V1 = str_remove(V1, "n") %>% as.numeric()+1,
                             V1 = as.character(V1)), by = c("node"="V1")) %>%
          mutate(class = factor(class),
                 sub_class = factor(sub_class))
        
        #a couple of the methods have twice as many dimensions but half of them are doubles.
        #This means the matrices are not full rank
        #ensureing only the first half of the variables are taken means that the matrices become full rank again
        number_of_columns <-3+ number_of_variables_df
        
        class_formula <- as.formula(paste("class", 
                                          paste(names(test_df)[4:number_of_columns], collapse=" + "), sep=" ~ "))
        sub_class_formula <- as.formula(paste("sub_class", 
                                              paste(names(test_df)[4:number_of_columns], collapse=" + "), sep=" ~ "))
        
        #class_mod <- svm(class ~  dimension_2, test_df, kernel = "linear")
        class_mod <- vglm( class_formula , family=multinomial, data = test_df)
        
        #sub_mod <- svm(sub_class ~  dimension_2, test_df, kernel = "linear")
        sub_mod <- vglm(sub_class_formula, family=multinomial, data = test_df)
        
        class_preds_vect <- predict(class_mod, decision.values = TRUE, type = "response") %>% 
          { colnames(.)[apply(., 1, which.max)]}
        sub_preds_vect <- predict(sub_mod, decision.values = TRUE, type = "response") %>% 
          { colnames(.)[apply(., 1, which.max)]}
        
        test_df <- test_df %>%
          mutate(class_preds =  factor(class_preds_vect, levels = c("A", "B")),
                 sub_preds  = factor(sub_preds_vect, levels = c("A_1", "A_2", "B_1", "B_2")))
        
      } 
      # test_df <- test_df %>%
      #   mutate(class_preds = predict(class_mod, decision.values = TRUE),
      #          sub_preds  = predict(sub_mod, decision.values = TRUE))
      
      bind_rows(peels_metrics(test_df, truth = class, estimate = class_preds) %>%
                  mutate(model_type = "class"),
                peels_metrics(test_df, truth = sub_class, estimate = sub_preds)%>%
                  mutate(model_type = "sub_class")
      ) %>%
        mutate(embeddings_method = graph_data[1],
               embeddings_dimensions = graph_data[3],
               graph_id = as.integer(graph_data[6]),
               type = str_remove(graph_data[8], pattern = ".csv"))
      
    })
  
  DGI_embeds_agg <- DGI_embedded_files  %>%
    map_df(~{
      
      print(.x)
      embedded_path <- .x#embedded_files[50]
      
      #split the file name to get the graph information
      graph_data <- str_split(basename(embedded_path), pattern = "_", simplify = T )
      
      #read the csv in from the folder
      csv_df <- read.csv(embedded_path, header = FALSE, skip = 1) #col_names = FALSE)
      
      #add the embedding values to the matching graph
      g_temp <- multi_quintet[[as.integer(graph_data[6])]] %>%
        set_vertex_attr(., "dimension_1", value =  csv_df %>% pull(2)) %>%
        set_vertex_attr(., "dimension_2", value =  csv_df %>% pull(3))
      
      #create dataframe to perform linear separation analysis on
      test_df <- as_data_frame(g_temp, what = "vertices") %>%
        mutate(class = factor(class),
               sub_class = factor(sub_class)) %>%
        mutate(embeddings_method = graph_data[1],
               embeddings_dimensions = graph_data[3],
               graph_id = as.integer(graph_data[6]),
               type = str_remove(graph_data[8], pattern = ".csv"))
      
      
    }) %>%
    group_by(embeddings_method, graph_id, type, embeddings_dimensions) %>%
    summarise(dimension_1 = mean(dimension_1),
              dimension_2 = mean(dimension_2))
  
  
  out_full <- bind_rows(GEM_embeds, 
                        SETSe_embeds, 
                        node_perf,
                        DGI_embeds %>% filter(.estimate != 0)
  )
  
  out_agg <- bind_rows(GEM_embeds_agg, n2v_embeds_agg, node_data_agg, DGI_embeds_agg) 
  
  out <- list(full = out_full, agg = out_agg)
  
  return(out)
  
}