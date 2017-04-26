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
  make_option( c("-t", "--train"), action = "store_true", dest = "train"),
  make_option( c("-e", "--experiment"), action = "store_false", dest = "train"),
  make_option( c("-c", "--compare"), action = "store_true", dest = "compare", default = FALSE)
);

opt_parser <- OptionParser(option_list=option_list);
opt        <- parse_args(opt_parser);


#' Call InputParser 
inputparser <- new('InputParser')
inputparser$parseCommand(options = opt)
