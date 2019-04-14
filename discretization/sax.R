

library(jmotif)
library(plyr)
library(dplyr)
library(qdapTools)
library(data.table)

source(file.path('auxiliar', 'formatting.R'))

### data in panel format

discretize <- function(panel_data, alphabet_size, paa_size){
  
  
  # Original data is in Horizontal Format:
  #MTS_data <- read.csv(paste(base_filename, "_APPROPRIATE.csv", sep=""))
  
  #list[panel_data, n_variables] <- parseToPanel(MTS_data)
  
  #c(panel_data, n_variables)
  
  # Transform into a DataFrame
  p_data <- data.frame(panel_data)
  
  # Separate Subjects
  DATA_BY_SUBJECT<-split(p_data, p_data$subject_id)
  
  final_data_discrete <- data.frame()
  subject_normalized <- list()
  subject_normalized_paa <- list()
  
  current_subject <- 0
  
  for(i in DATA_BY_SUBJECT){
    subject <- as.data.frame(i)
    
    current_subject <- current_subject + 1
    
    # Store to regroup later
    id <- subject$subject_id
    timestamp <- subject$timestamp
    
    subject$subject_id <- NULL
    subject$timestamp <- NULL
    
    subject <- transpose(subject)
    
    subject <- as.list(as.data.frame(t(subject))) # Convert each row of the dataframe to a list (each list is a timeseries sequence)
    
    # Normalize the data (mean=0 and std=1)
    #subject_normalized <- llply(subject, function(x){znorm(x, threshold = 0.01)})
    subject_normalized <- append(subject_normalized,list(llply(subject, function(x){znorm(x, threshold = 0)}))) # 0.01
    
    
    # Reduce the dimensionality using PAA
    subject_normalized_paa <- append(subject_normalized_paa,list(llply(subject_normalized[current_subject][[1]], function(x){paa(x, paa_size)})))
    
    # Use the reduced data and convert it to symbolic using a certain alphabet size
    #subject <- llply(subject_normalized[current_subject][[1]], function(x){series_to_string(x, alphabet_size)})
    subject <- llply(subject_normalized_paa[current_subject][[1]], function(x){series_to_string(x, alphabet_size)}) 
    
    # Turn each character into an element, columns represent position on string
    subject <- llply(subject, function(x){strsplit (x, "")}) 
    
    subject_dataframe = ldply(transpose(unlist(subject, recursive = FALSE )))
    
    
    subject_regroup <- data.frame(cbind(id[1:nrow(subject_dataframe)], timestamp[1:nrow(subject_dataframe)], subject_dataframe))
    names(subject_regroup)[1] <- "subject_id"
    names(subject_regroup)[2] <- "timestamp"
    
    # Correct format for Java Algorithm:
    subject_data_horizontal <- parseToHorizontal(subject_regroup)
    
    final_data_discrete <- rbind.data.frame(final_data_discrete, subject_data_horizontal)
    
  }
  
  # Write CSV
  #write.csv(final_data_discrete, file = paste(base_filename, "_APPROPRIATE_DISCRETE.csv", sep=""), row.names = FALSE)
  out <- list(final_data_discrete, subject_normalized, timestamp)
  
  return(out) # subject_normalized, timestamp are needed for the plot
  
}











mean_std_plot <- function(subject_normalized, timestamp, n_variables, alphabet_size){
  
  ###### AUXILIARY COMPUTATIONS TO TRANSFORM THE NORMALIZED DATA TO PANEL DATA
  normalized_MTS_data <- data.frame()
  
  for(i in 1:length(subject_normalized)){
    subject_id <- array(i, dim = length(timestamp)) # timestamp was declared earlier when handeling each subject
    normalized_MTS_data <- rbind(normalized_MTS_data,data.frame(cbind(subject_id, timestamp, data.frame(subject_normalized[[i]]))))
  }
  
  ############## THIS DEPENDS ON THE NUMBER OF VARIABLES AND THE NUMBER OF ROWS OF THE DATAFRAMES:
  normalized_for_plot <- normalized_MTS_data
  normalized_for_plot$subject_id <- NULL
  
  normalized_for_plot_V <- list()
  
  aux <- data.frame()
  aux3 <- rep(0, 0)
  for(i in 1:n_variables){
    v_i <- paste("V",i, sep = "") # name of the variable
    aux2 <- data.frame(normalized_for_plot$timestamp, normalized_for_plot %>% select_(v_i))# normalized_for_plot$V1
    colnames(aux2) <- c("timestamp", "value")
    
    aux <- rbind(aux, aux2)
    
    aux3 <- c(aux3, rep(v_i,nrow(aux2)))
  }
  normalized_for_plot <- aux
  normalized_for_plot$variable <- aux3
  

  return(
    plot_combined <- ggplot(normalized_for_plot, aes(timestamp,value,group=variable,col=variable)) + 
      stat_summary(fun.y = 'mean', geom = 'line', size = 1.3) +
      stat_summary(fun.data = 'mean_sdl', geom = 'ribbon', alpha = 0.2) +
      geom_hline(yintercept = alphabet_to_cuts(alphabet_size)[2:alphabet_size], color = 'magenta', linetype="dashed") +
      ggtitle("Mean and standard deviation of each timestamp for every normalized variable") +
      theme(plot.title = element_text(hjust = 0.5, color="black", size=12, face="bold")) +
      xlab("Timestamp") + ylab("Normalized value")
  )
  
  
}














###### AUXILIARY FUNCTION FOR DISCRETIZATION OF MISSING VALUES THROUGH COMPLETE DATA
### Datasets must possess the subject_ids for identification
discretize_missing <- function(original_complete_data, discrete_complete_data, missing_data, variable_name){
  
  
  ### Much easier if the datasets are in panel format:
  discrete_complete_panel <- data.frame(parseToPanel(discrete_complete_data))
  original_complete_panel <- data.frame(parseToPanel(original_complete_data))
  
  # First see how many symbols:
  symbols <- unlist(unique(discrete_complete_panel %>% select_(.dots = "V1")))
  print(typeof(symbols))
 # symbols <- unfactor(symbols)
  selection <- cbind(c("subject_id", "timestamp"), "V1")  
  
  boundaries <- data.frame()
  
  for(symbol in symbols){
    print(symbol)
    ### Rows with a particular symbol:
    aux <- discrete_complete_panel %>% select_(.dots = selection)
    #print(aux)
    #rows <- aux[apply(aux[, -1], MARGIN = 1, function(x) any(x %in% symbol)), ]
    original_values <- original_complete_panel[apply(aux[, -1], MARGIN = 1, function(x) any(x %in% symbol)), ]
    print(original_values)
    
    ### Array of all the observations retrieved of that symbol:
    symbol_array <- original_values %>% select_(.dots = variable_name)
    print(symbol_array)
    symbol_array <- as.vector(symbol_array)
    #print(symbol_array)
    symbol_array <- sort(as.numeric(unlist(symbol_array)))
    print(symbol_array)
    
    print(symbol_array[1])
    
    print(symbol_array[length(symbol_array)])
    
    
    aux2 <- data.frame(symbol = symbol,low_limit = symbol_array[1], high_limit = symbol_array[length(symbol_array)])
    boundaries <- rbind(boundaries, aux2)
    
    ### Retrieve the observations of the original dataset
    #original_values <- original_complete_panel[aux$subject_id %in% rows$subject_id,]
      
  
    ### Retrieve the associate original values
    
    ### Sort the values and retrieve the first and last
    ### This defines the boundaries of the region for that symbol in the original space (no normalization)
  
  }
  
  print(boundaries)
  
  
  
  
  # ### Maybe see the boundaries of each symbol of the un-normalized original data
  # 
  # # First see how many symbols:
  # symbols <- unique(discrete_complete_data)
  # 
  # # Check the first and last of each symbol in original complete dataset
  # for(symbol in symbols){
  #   
  #   # For each subject retrieve observations of the symbol
  #   for(subject in discrete_complete_data$subject_id){
  #     
  #     aux <- 
  #     
  #     symbol_obs <- cbind(symbol_obs, )
  #   }
  #   
  #   # Retrive the observations classified as this symbol
  #   discrete_complete_data[]
  #   
  #   
  # }
  # 
  # # Values of missing falling in those regions assume that value
  # 
  # 
  # return(discrete_missing_data)
}





