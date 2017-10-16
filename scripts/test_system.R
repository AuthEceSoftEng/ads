# This script demonstrates how experiment.R should be called in order to perform an experiment
# on a dataset. In particular, it performs an experiment for each dataset under
# [workspace/datasets_repo](workspace/datasets_repo).
rm(list=ls())

# get all files in workspace/datasets_repo
files_list = list.files(path = "workspace/datasets_repo", pattern="*.csv", recursive = TRUE)


# perform experiments
for (i in seq(1,length(files_list))) {
  project_name <- substr(files_list[[i]], start = 1, stop = nchar(files_list[[i]]) -4 )
  command      <- paste("Rscript experiment.R -e -d", files_list[[i]],  sep = " ")
  system(command)
}