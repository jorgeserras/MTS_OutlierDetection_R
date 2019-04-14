
library("rJava")

### Returns the transition and subject scores of a MTS dataset in Horizontal format, check website for more info
### Receives the path where files are written and DBN parameters to model
### score_function parameter must be either "ll" for Log-Likelihood or "mdl" for MDL
DBN_scoring <- function(data_file_name, write_path, markov_lag, previous_parents, checkbox_stationary, score_function){
  
  #jDialog <- J('com/github/tDBN/cli/Model_training_outlier_detection')
  jDialog <- 'com/github/tDBN/cli/Model_training_outlier_detection'
  
  file_path <- paste(write_path, data_file_name, sep = "")
  
  #j_filepath <- new(J("java.lang.String"), file_path)
  #j_filepath <- new(J("java.lang.String"), write_path)
  
  #j_score <- new(J("java.lang.String"), "mdl") #Log-Likelihood
  
  #java_object <- new(jDialog, j_filepath, as.integer(markov_lag),as.integer(previous_parents),checkbox_stationary,j_score)
  
  
  j_filepath <- .jnew("java.lang.String", file_path)
  j_score <- .jnew("java.lang.String", score_function)
  #j_score <- .jnew("java.lang.String", score_function)
  java_object <- .jnew(jDialog, j_filepath, as.integer(markov_lag),as.integer(previous_parents),checkbox_stationary,j_score, check = TRUE)
  
  
  write_path <- getwd() # Output files go to this folder
  transition_scores <- as.data.frame(read.csv(file = paste(write_path, "/scores_transition_output.csv", sep = ""), header=TRUE))
  subject_scores <- as.data.frame(read.csv(file = paste(write_path, "/subject_scores_output.csv", sep = ""), header=TRUE))
  
  #transition_scores<<-read.csv("scores_transition_output.csv")
  #subject_scores<<-read.csv("subject_scores_output.csv")
  n_subjects <- nrow(subject_scores)
  
  return(list(transition_scores,subject_scores,n_subjects))
  
}