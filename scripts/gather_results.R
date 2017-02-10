rm(list=ls())

setwd("automl-software")

projects_list <- list.dirs(path = "workspace/project_test_knn/new_new", full.names = TRUE, recursive = FALSE)
projects_list

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
write.csv(as.data.frame(accuracies), "/home/elena/R_ws/automl/HPP_train/train_knn/enseble_accuracies_new.csv" )
# save accuracies with names of files