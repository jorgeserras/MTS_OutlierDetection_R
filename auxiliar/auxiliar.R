
library(ggplot2)
library(reshape2)

source(file.path('auxiliar', 'formatting.R'))

### Receives an array of subjects scores with respective subject_ids and a threshold, outputting the final verdict for each subject
### Scores must be from minus infinity to zero
### 1 - outlier; 0 - normal
classify_subjects <- function(subject_scores, threshold){
  
  aux_outlier <- subject_scores
  subject_id <- aux_outlier$subject_id
  aux_outlier$subject_id <- NULL
  
  #### This resolves the problem of having anomaly scores of 0 (positive anomaly scores are impossible):
  if(length(aux_outlier[aux_outlier<=threshold])==0){ # Only normal
    aux_outlier[aux_outlier>threshold & aux_outlier!=1] <- 0 # Normal
  }else{
    aux_outlier[aux_outlier<=threshold] <- 1 # Anomalies
    if(length(aux_outlier[aux_outlier>threshold & aux_outlier!=1])!=0) # There are normals
      aux_outlier[aux_outlier>threshold & aux_outlier!=1] <- 0 # Normal
  }
  
  aux_outlier <- cbind(subject_id, aux_outlier)
  colnames(aux_outlier) <- c("subject_id", "outlier")
  return(aux_outlier)
} 



### Receives an array of transition scores with respective subject_ids and a threshold, outputting the final verdict for each transition
### Scores must be from minus infinity to zero
### 1 - outlier; 0 - normal
classify_transitions <- function(transition_scores, threshold){
  
  aux_outlier <- transition_scores
  subject_id <- aux_outlier$subject_id
  aux_outlier$subject_id <- NULL
  
  #### This resolves the problem of having anomaly scores of 0 (positive anomaly scores are impossible):
  if(length(aux_outlier[aux_outlier<=threshold])==0){ # Only normal
    aux_outlier[aux_outlier>threshold & aux_outlier!=1] <- 0 # Normal
  }else{
    aux_outlier[aux_outlier<=threshold] <- 1 # Anomalies
    if(length(aux_outlier[aux_outlier>threshold & aux_outlier!=1])!=0) # There are normals
      aux_outlier[aux_outlier>threshold & aux_outlier!=1] <- 0 # Normal
  }
  
  aux_outlier <- cbind(subject_id, aux_outlier)
  return(aux_outlier)
} 


# aux_subj <- subject_scores
# aux_subj$outlier <- NULL
# 
# aux_subj$outlier=factor(c(0,1))
# if(nrow(aux_subj[aux_subj$score<=Tukey_threshold_subje,])==0){ # No anomalies
#   aux_subj[aux_subj$score>Tukey_threshold_subje & aux_subj$score!=1,]$outlier <- 0 # Only Normal
# }else{
#   aux_subj[aux_subj$score<=Tukey_threshold_subje,]$outlier <- 1 # Anomalies
#   if(nrow(aux_subj[aux_subj$score>Tukey_threshold_subje & aux_subj$score!=1,])!=0){ #Are there normals?
#     aux_subj[aux_subj$score>Tukey_threshold_subje & aux_subj$score!=1,]$outlier <- 0 # Normal
#   }
# }





#### Considers data concerns solely one variable representing TS
plot_anomalous_subjects <- function(original_data, classified_data, variable_name){
  
  original_data_panel <- data.frame(parseToPanel(original_data))
  original_data_panel$X1L <- NULL
  
  original_data_panel <- data.frame(original_data_panel$subject_id,original_data_panel$timestamp,original_data_panel %>% select_(variable_name))
  
  aux <- data.frame()
  aux3 <- rep(0, 0)

  aux2 <- original_data_panel
  colnames(aux2) <- c("subject_id" ,"timestamp", "value")
    
  aux <- rbind(aux, aux2)
    
  aux3 <- c(aux3, rep(variable_name,nrow(aux2)))
  original_data_panel <- aux
  original_data_panel$variable <- aux3
  
  #print(classified_data)
  selection <- classified_data[classified_data$outlier == 1,]
  selection <- selection$subject_id
  
  #print(selection)
  original_data_panel <- original_data_panel[original_data_panel$subject_id %in% selection,]
  
  original_data_panel$variable <- NULL
  #print(original_data_panel)
  
  ytext <- paste(variable_name, " values",sep = "")
  
  return(ggplot(original_data_panel, aes(x = timestamp,y = value, group = subject_id,  col = subject_id)) + 
    ggtitle("Anomalous subjects (TS)") + geom_line() +
    theme(plot.title = element_text(color="black", size=12, face="bold")) +
    xlab("Timestamp") + ylab(ytext))
  
}



#### Considers data concerns solely one variable representing TS
### subjects should be in format: c("4","18", ...) where each number specifies an existing subject_id
plot_subjects <- function(original_data, subjects, variable_name){
  
  original_data_panel <- data.frame(parseToPanel(original_data))
  original_data_panel$X1L <- NULL
  
  original_data_panel <- data.frame(original_data_panel$subject_id,original_data_panel$timestamp,original_data_panel %>% select_(variable_name))
  
  aux <- data.frame()
  aux3 <- rep(0, 0)
  
  aux2 <- original_data_panel
  colnames(aux2) <- c("subject_id" ,"timestamp", "value")
  
  aux <- rbind(aux, aux2)
  
  aux3 <- c(aux3, rep(variable_name,nrow(aux2)))
  original_data_panel <- aux
  original_data_panel$variable <- aux3
  
  #print(classified_data)
  #selection <- subjects[classified_data$outlier == 1,]
  selection <- subjects
  
  #print(selection)
  original_data_panel <- original_data_panel[original_data_panel$subject_id %in% selection,]
  
  original_data_panel$variable <- NULL
  #print(original_data_panel)
  
  ytext <- paste(variable_name, " values",sep = "")
  
  return(ggplot(original_data_panel, aes(x = timestamp,y = value, group = subject_id,  col = subject_id)) + 
           ggtitle("Subjects (TS)") + geom_line() +
           theme(plot.title = element_text(color="black", size=12, face="bold")) +
           xlab("Timestamp") + ylab(ytext))
  
}




#### Considers that data concerns solely one variable representing TS
## If the results come from a DBN specify the Markov lag used in the model, otherwise let it be zero (default)
## Remember that data marked as outlier refers to the whole transition in case of a DBN and not only the values of that specific slice
## An entire anomalous transition is red, which ends in the last time slice generated by that transition
plot_anomalous_transitions <- function(original_data, classified_data, Markov_lag, variable_name){
  
  
  original_data_panel <- data.frame(parseToPanel(original_data))
  original_data_panel$X1L <- NULL
  
  original_data_panel$outlier <- "A"
  
  subject_id <- data.frame(subject_id = classified_data$subject_id)

  transition <- data.frame()
  for(i in 2:ncol(classified_data)){
    #print(i)
    subjects_with_anomalies <- data.frame()
    transition <- classified_data[,i]
    transition <- cbind(subject_id,transition)
    ### timestamp com MarkovLag decidem onde fica o outlier = 1
    ### Change specific position:
    transition_number <- i+Markov_lag-1
    #subjects_with_anomalies <- c(transition[transition$transition == 1,])
    #subjects_with_anomalies <- unfactor(subjects_with_anomalies$subject_id)
    subjects_with_anomalies <- transition[transition$transition == 1,]$subject_id
   # subjects_with_anomalies <- ((unfactor(subjects_with_anomalies))
    #print(subjects_with_anomalies)
    #print(original_data_panel[original_data_panel$subject_id %in% subjects_with_anomalies,])
    if(length(subjects_with_anomalies)==0){
      #pass
    }else{
      #original_data_panel[which(original_data_panel$subject_id %in% subjects_with_anomalies & original_data_panel$timestamp == transition_number),]$outlier <- "B"
      original_data_panel[which(original_data_panel$subject_id %in% subjects_with_anomalies & original_data_panel$timestamp < transition_number & original_data_panel$timestamp >= transition_number-Markov_lag),]$outlier <- "B"
      
      #subjects_anomalies <- original_data_panel[original_data_panel$subject_id %in% subjects_with_anomalies,]
      #subjects_anomalies <- subjects_anomalies[subjects_anomalies$timestamp == transition_number,]
      #&& original_data_panel$timestamp == transition_number
      #subjects_anomalies$outlier <- 1
      #print(subjects_anomalies)
    }
    
    transition <- data.frame()
  }
  colnames(original_data_panel) <- c("subject_id" ,"timestamp", "value", "outlier")
  #print(original_data_panel)
  
  aux <- parseToHorizontal(original_data_panel)
  #print(aux)
  
  sel <- apply(aux[,colnames(aux)],1,function(row) "B" %in% row)
  
  aux <- parseToPanel(aux[sel,])
  
  plot_data <- data.frame(aux[[1]])
  
  ytext <- paste(variable_name, " values",sep = "")
  
  title <- paste("Subjects (TS) with anomalous transitions marked, using windows of size ",Markov_lag+1,sep = "")
  
  ### "A" - normal , "B" - outlier
  return(ggplot(plot_data, aes(x = timestamp,y = value, group = subject_id,  col = outlier)) + 
           ggtitle(title) + geom_line() + geom_point() +
           scale_color_manual(values = c("A" = "black", "B" = "red"), breaks=c("A", "B"), labels=c("Normal", "Anomalous"), name="Class") +
           theme(panel.background = element_rect(fill = "lightblue",
                                                 colour = "lightblue",
                                                 size = 0.5, linetype = "solid"), plot.title = element_text(color="black", size=12, face="bold")) +
           xlab("Timestamp") + ylab(ytext) # + scale_size_continuous(name="Class", labels=c("normal", "anomalous")) #breaks=c("A", "B")
         )
}



#### Only for subject classification results
### Receives a data.frame containing the classification of each subject where one column "subject_id" serves as identification
### and another column "outlier" specifies the class (0 - normal, 1 - outlier)
###
### Outputs the subject_id of the outlier subjects
retrieve_anomalous_subject_ids <- function(classified_subject_data){
  anomalous_ids <- data.frame(subject_id = classified_subject_data[classified_subject_data$outlier == 1, ]$subject_id)
  return(anomalous_ids)
}



#### Only for transition classification results
### Receives a data.frame containing the classification of each transition where the first column "subject_id" serves as identification of each subject
### and in each column, the class of each transition is specified (0 - normal, 1 - outlier)
###
### Outputs the subject_id of subjects containing atleast one anomalous transition
retrieve_subjects_with_anomalous_transitions <- function(classified_transition_data){
  anomalous_ids <- data.frame(subject_id = classified_transition_data[rowSums(classified_transition_data[,2:ncol(classified_transition_data)])>0, ]$subject_id)
  return(anomalous_ids)
}

