rm(list=ls())
setwd("automl-software")
library(testthat)
source("build_script.R")
test_results <- test_dir("optimizer/testing", reporter = "summary")

