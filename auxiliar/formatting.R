

library("dplyr")

#----------------- FORMATTING FUNCTIONS -----------------# 

parseToPanel <- function(data){
  # Parsing the input data
  col <- colnames(data)
  numVariables <- 0
  variables <- c()
  time <- c()
  for(i in 2:length(col)){
    aux <- strsplit(col[i], "__")
    time = append(time, strtoi(aux[[1]][[2]]))
    variables = append(variables,aux[[1]][[1]])
  }
  
  time = unique(time)
  variables = unique(variables)
  subject_id <- data[,1]
  subject_id_panel <- c()
  
  for(i in 1:dim(data)[1]){
    x = rep(subject_id[i], length(time))
    subject_id_panel <- append(subject_id_panel, x)
  }  
  
  time_panel <- rep(time, dim(data)[1])
  
  panel_data = cbind(subject_id_panel, time_panel)
  for (variable in variables) {
    
    str <- c()
    for(t in time){
      str <- append(str,paste(variable,t, sep = "__"))
    }
    panel_data = cbind(panel_data, as.vector(t(data[,str])))
    
  }
  colnames(panel_data) =  c(col[1], "timestamp", variables)
  
  return(list(panel_data, length(variables))) ############ NEED TO KNOW NUMBER OF VARIABLES
}


parseToHorizontal <- function(data){
  
  data <- as.data.frame(data)
  data_horizontal <- c()
  col <- colnames(data)
  head <- c()
  head <- append(head, col[1])
  col <- col[-c(1,2)]
  time <- unlist(unique(data["timestamp"]))
  for(t in time){
    for(v in col){
      str <- paste(v, t, sep="__")
      head <- append(head, str)
    }
  }
  
  
  subjects <- unlist(unique(data["subject_id"]))
  for(subject in subjects){
    data_aux <- c()
    data_aux <- append(data_aux, subject)
    aux <- subset(data, subject_id == subject)
    aux <- as.vector(t(aux[,-c(1,2)]))
    data_aux <- append(data_aux, aux)
    data_horizontal <- cbind(data_horizontal, data_aux)
  }
  data_horizontal <- as.matrix(t(data_horizontal))
  colnames(data_horizontal) <- c(head)
  rownames(data_horizontal) <- c()
  # data_horizontal[,-c(1)] <- format(data_horizontal[,-c(1)], nsmall = 1)
  # data_horizontal[,1] <- format(data_horizontal[,1], nsmall = 0)
  return(data_horizontal)
}



#### Outputs cleaned subsets of the original data in the correct format
### Cleaning includes removing rows full of NAs and creating a subset with rows without missing values in mandatory attributes 
### PARAMETERS:
### data - data.frame with MTS data, see example dataset. Identifier must be named "subject_id"
### variable_names - column names to select
### new_variable_names - The new names after selecting the variables (maintaining relative order), these show be in format "NAME__t" if belonging to a MTS, t is the timestamp
### mandatory_variables - any row with a missing value from atleast one mandatory variable is dropped to form another subset, names of mandatory variables must be included in new_variable_names
### full_TS - rows without any value from this time series are dropped, should be in the format c("NAME__1", "NAME__2"...) and this values must be in new_variable_names
clean_data <- function(data, variable_names, new_variable_names, mandatory_variables, full_TS){
  # Solely select a subset of variables:
  subset.data <- data[variable_names]
  # Remove rows full of NA:
  subset.data <- subset.data[rowSums(is.na(subset.data)) != ncol(subset.data), ]
  # New variable names
  colnames(subset.data) <- new_variable_names
  
  ### From the subset, remove subjects with unknown mandatory variables
  if(!is.null(mandatory_variables)){
    selection <- cbind(c("subject_id"), mandatory_variables)
    drop <- subset.data %>% select_(.dots = selection)
    #drop <- unique(drop$subject_id)
    drop <- drop[(rowSums(is.na(drop)) > 0), ]
    
    subset.data.mandatory <- subset.data[!(subset.data$subject_id %in% drop$subject_id),]
    
    colnames(subset.data.mandatory) <- new_variable_names
    
    #select only subject id and TS
    selection <- cbind(c("subject_id"), full_TS)
    drop <- subset.data.mandatory %>% select_(.dots = selection)
    
    drop <- drop[(rowSums(is.na(drop)) + 1) != ncol(drop), ]
    aux <- drop$subject_id
    subset.data.mandatory <- subset.data.mandatory[subset.data.mandatory$subject_id %in% aux,]
    
  }else{
    subset.data.mandatory <- subset.data # If there is no mandatory constraint, just use the default subset
  }
  
  ### Only subjects with full information:
  subset.data.fulldata <- subset.data.mandatory[complete.cases(subset.data.mandatory), ]
  
  
  ### Patient ids with missing values:
  subset.data.missing <- subset.data.mandatory[(rowSums(is.na(subset.data.mandatory)) > 0), ]
  #subjects_with_missing <- subset.data.missing$subject_id
  

  return(list(subset.data, subset.data.mandatory, subset.data.fulldata, subset.data.missing))

}


### Receives a dataset and returns subjects with less than n missing values
###
less_n_missing <- function(data, n){
  
  subset.data.atleastn <- data[(rowSums(is.na(data)) < n), ]
  #subjects_atleast_n_values <- subset.data.atleastn$subject_id
  
  return(subset.data.atleastn)
}


### Receives a dataset and the subject ids to remove
###
remove_subjects <- function(data, subjects){
  
  data <- data[!(data$subject_id %in% subjects),]
  
  ######## THE LAST GUY WITH 116 years is not CORRECT, REMOVE
  #cancerSys.subset.data_vars <- cancerSys.subset.data_vars[!(cancerSys.subset.data_vars$subject_id == "177"),]
  #cancerSys.subset.data_numeric <- cancerSys.subset.data[!(cancerSys.subset.data$subject_id == "177"),]
  
  return(data)
}




### Receives a dataset and removes subjects with certain observations
### Any subject wich variables possess one of the specified values is removed
remove_values <- function(data, variables, values){
  
  selection <- cbind(c("subject_id"), variables)  
  drop <- data %>% select_(.dots = selection)
  rows <- drop[apply(drop[, -1], MARGIN = 1, function(x) any(x %in% values)), ]
  drop <- data[!(drop$subject_id %in% rows$subject_id),]
  
  return(drop)
}


### Receives a dataset composed by TS of a common variable being the first column the "subject_id"
## Non-numeric values are not allowed
log_MTS <- function(data){ # Univariate TS only
  data <- as.data.frame(data)
  data[, 2:ncol(data)] <- log(data[2:ncol(data)]) # First column is "subject_id"
  return(data)
}

### Receives a dataframe composed by scores with "subject_id" column and returns an array in time series format with every score
to_score_array <- function(scores){
  scores$subject_id <- NULL
  score_values <- as.vector(t(scores)) ## array with all scores in serie
  return(score_values)
}



### The aim is to have a clean .csv file in the desired format format:
### USAGE: 
###
###    MTS_data - must be a data.frame and in the format contrary to the one wished
###    format - desired output format: is "H" for Horizontal or any other for Panel
###
format_data <- function(MTS_data,format){
  
  if(format=="H"){
    tryCatch(
      {
        aux_data <- parseToHorizontal(MTS_data)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(paste('The input file is not on panel format or presents other issues. Try Horizontal format and check for discrepancies.')))
      }
    )
  }else{
    tryCatch(
      {
        aux_data <- parseToPanel(MTS_data)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(paste('The input file is not on horizontal format or presents other issues. Try Panel format and check for discrepancies.')))
      }
    )
  }
  
  data_output <- data.frame(aux_data)
  
  return(data_output)
  
}


