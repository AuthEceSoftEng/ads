rm(list=ls())

setwd("automl-software")

projects_list <- list.dirs(path = "workspace/project_test_knn", full.names = TRUE, recursive = FALSE)
projects_list
files_list = list.files(path = "workspace/project_test_knn", pattern="experiment_info.RData", recursive = TRUE)

accuracies <- c()
for (i in 1:length(projects_list)) {
  # get name of experiment-info file
  info_file <- paste(projects_list[[i]], "experiment_info.Rdata", sep = "/")
  # load experiment-info file
  load(info_file)
  accuracy <- data$testing$model_validation$metrics$Accuracy
  # keep accuracy
  accuracies <- c(accuracies, accuracy )
  
  
}

# save accuracies with names of files