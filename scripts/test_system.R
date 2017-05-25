rm(list=ls())

# get all files in workspace/datasets_repo
files_list = list.files(path = "workspace/datasets_repo", pattern="*.csv", recursive = TRUE)
#for each one
for (i in c(3,5,6,7)) {
  project_name <- substr(files_list[[i]], start = 1, stop = nchar(files_list[[i]]) -4 )
 # project_name <- paste("project_test_knn", project_name, sep ="/")
  command <- paste("Rscript experiment.R -e -d", files_list[[i]],  sep = " ")
  cat(command)
  cat("\n")
  system(command)
}
  # make base-command
  # append -d (project is automaticall appended)
  # append -projec
1,2,4,8,13,14
rm(list=ls())

# get all files in workspace/datasets_repo
files_list = list.files(path = "workspace/datasets_repo", pattern="*.csv", re$
                          cat(files_list)
                        #for each one
                        for (i in c(3,5,6,7,9,10,11,12))) {
                          project_name <- substr(files_list[[i]], start = 1, stop = nchar(files_list[$
                                                                                                       # project_name <- paste("project_test_knn", project_name, sep ="/")
                                                                                                       command <- paste("Rscript experiment.R -e -d", files_list[[i]],  sep = " ")
                                                                                                     cat(command)
                                                                                                     cat("\n")
                                                                                                     system(command)
                        }
                        # make base-command
                        # append -d (project is automaticall appended)
                        # append -projec
                        