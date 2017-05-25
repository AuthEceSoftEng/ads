rm(list=ls())

# get all files in workspace/datasets_repo
files_list = list.files(path = "workspace/datasets_repo", pattern="*.csv", recursive = TRUE)
cat(files_list)
#for each one

for (i in c(15,19,21)) {
  project_name <- substr(files_list[[i]], start = 1, stop = nchar(files_list[[i]]) -4 )
 # project_name <- paste("project_test_knn", project_name, sep ="/")
  command <- paste("Rscript experiment.R -e -d", files_list[[i]],  sep = " ")
  cat(command)
  cat("\n")
  system(command)
}

