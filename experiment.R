#' The script for running an experiment. This is the interface of automl software. It offfers the functionalities of 
#' training the system, performing an experiment for a particular dataset by defining appropriate flags. 
#' It sources build_script.R to set up the workspace. <br> <br> <br>

#' Ensure that .libpaths of Rscript.exe and R.exe agree
if(length(.libPaths()) == 1){
  # We're in Rscript.exe
  possible_lib_paths <- file.path(Sys.getenv(c('USERPROFILE','R_USER')),
                                  "R","win-library",
                                  paste(R.version$major,
                                        substr(R.version$minor,1,1),
                                        sep='.'))
  indx <- which(file.exists(possible_lib_paths))
  if(length(indx)){
    .libPaths(possible_lib_paths[indx[1]])
  }
  # CLEAN UP
  rm(indx,possible_lib_paths)
}

#' Set up workspace
source("build_script.R")

#' Load user's configuration 


#' Define command-line options
option_list <- list(
  make_option( c("-d", "--dataset"), type = "character",
               default = "baboon_mating.csv", help = "dataset file name",
               metavar = "character"),
  make_option( c("-p", "--project"), type = "character", help = "name of new project",
               default = NULL, metavar = "character"),
  make_option( c("-w", "--workspace"), type = "character",
               default = workspace_dir, help = "name of workspace directory",
               metavar = "character"), 
  make_option( c("-t", "--train"), action = "store_true", dest = "train", default = FALSE),
  make_option( c("-exp", "--experiment"), action = "store_true", dest = "experiment", default = FALSE),
  make_option( c("-c", "--compare"), action = "store_true", dest = "compare", default = FALSE),
  make_option( c("-co", "--compress"), action = "store_true", dest = "compress", default = TRUE),
  make_option( c("-n", "--normalize"), type = "character", help = "type of normalization",
               default = NA, metavar = "zscore or minmax"),
  make_option( c("-ua", "--unknown_action"), type = "character", help = "deal with unknown values",
               default = NA, metavar = "replace or remove"),
  make_option( c("-ur", "--unknown_replace"), type = "character", help = "replace value for unknown",
               default = NA, metavar = "numeric"),
  make_option( c("-ia", "--inf_action"), type = "character", help = "deal with infinite values",
               default = NA, metavar = "replace or remove"),
  make_option( c("-ir", "--inf_replace"), type = "character", help = "replace value for infinites",
               default = NA, metavar = "numeric"),
  make_option( c( "--dis_svm"), action = "store_true", dest = "dis_svm", default = FALSE),
  make_option( c("--dis_ann"), action = "store_true", dest = "dis_ann", default = FALSE),
  make_option( c("--dis_bayes"), action = "store_true", dest = "dis_bayes", default = TRUE),
  make_option( c("--dis_tree"), action = "store_true", dest = "dis_tree", default = FALSE),
  make_option( c( "--dis_knn"), action = "store_true", dest = "dis_knn", default = FALSE),
  make_option( c("-ns", "--ensemble_size"), type = "numeric", help = "size of ensemble",
               default = NA, metavar = "numeric"),
  make_option( c("-t_t", "--testing_technique"), type = "character", help = "testing technique",
               default = "holdout", metavar = "kfold or holdout or loocv"),
  make_option( c("-pm", "--performance"), type = "character", help = "metric used to evaluate performance",
               default = NA, metavar = "accuracy or auc"),
  make_option( c("-t_t_r", "--testing_technique_ratio"), type = "numeric", help = "ratio of training instances",
               default = NA , metavar = "numeric"),
  make_option( c("-bf", "--benchmark_file"), type = "character", help = "file containing benchmarks",
               default = NA , metavar = "csv"),
  make_option( c( "--predict"), action = "store_true", dest = "predict", default = FALSE),
  make_option( c("--cores"), type = "numeric", help = "number of cores",
               default = 1, metavar = "numeric")
);

opt_parser <- OptionParser(option_list=option_list);
opt        <- parse_args(opt_parser);


#' Call InputParser 
inputparser <- new('InputParser')
inputparser$parseCommand(options = opt)
