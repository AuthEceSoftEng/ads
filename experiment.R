#!/usr/bin/env Rscript
library("optparse")
library("methods")
source("build_script.R")
# create workspace (if not existed)
mainDir <- getwd()
subDir <- "workspace"
workspace_dir <- file.path(mainDir, subDir)
dir.create(workspace_dir, showWarnings = FALSE)
#setwd(file.path(mainDir, subDir))
#define command-line options
option_list = list(
  make_option( c("-d", "--dataset"), type = "character",
  default = "baboon_mating.csv", help = "dataset file name",
  metavar = "character"),
make_option( c("-p", "--project"), type = "character",
default = "my_first_experiment", help = "name of new project",
metavar = "character"),
make_option( c("-w", "--workspace"), type = "character",
             default = workspace_dir, help = "name of workspace directory",
             metavar = "character"),
make_option( c("-t", "--train"), action = "store_true", dest = "train"),
make_option( c("-e", "--experiment"), action = "store_false", dest = "train")
);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
opt

#call InputParser from package middleman
inputparser = new('InputParser')
inputparser$parseCommand(options = opt)
