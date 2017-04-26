#' A script that sets up automl's workspace. It installs missing packages and sources the files.
#' Run with `source("build_script.R")` from automl directory. <br> <br> <br>

#' Install and load missing  packages
if(!require(optparse))
{
  print("You are missing the package 'optparse', we will now try to install it...")
  install.packages("optparse")
  library(optparse)
}
if(!require(doParallel))
{
  print("You are missing the package 'doParallel', we will now try to install it...")
  install.packages("doParallel")
  library(doParallel)
}
if(!require(plyr))
{
  print("You are missing the package 'plyr', we will now try to install it...")
  install.packages("plyr")
  library(plyr)
}
if(!require(foreach))
{
  print("You are missing the package 'foreach', we will now try to install it...")
  install.packages("foreach")
  library(foreach)
}
if(!require(methods))
{
  print("You are missing the package 'methods', we will now try to install it...")
  install.packages("methods")
  library(methods)
}
if(!require(caret))
{
  print("You are missing the package 'caret', we will now try to install it...")
  install.packages("caret")
  library(caret)
}
if(!require(FactoMineR))
{
  print("You are missing the package 'FactoMineR', we will now try to install it...")
  install.packages("FactoMineR")
  library(FactoMineR)
}
if(!require(MASS))
{
  print("You are missing the package 'MASS', we will now try to install it...")
  install.packages("MASS")
  library(MASS)
}
if(!require(coin))
{
  print("You are missing the package 'coin', we will now try to install it...")
  install.packages("coin")
  library(coin)
}
if(!require(multcomp))
{
  print("You are missing the package 'multcomp', we will now try to install it...")
  install.packages("multcomp")
  library(multcomp)
}
if(!require(colorspace))
{
  print("You are missing the package 'colorspace', we will now try to install it...")
  install.packages("colorspace")
  library(colorspace)
}
if(!require(pROC))
{
  print("You are missing the package 'pROC', we will now try to install it...")
  install.packages("pROC")
  library(pROC)
}
if(!require(e1071))
{
  print("You are missing the package 'e1071', we will now try to install it...")
  install.packages("e1071")
  library(e1071)
}
if(!require(kernlab))
{
  print("You are missing the package 'kernlab', we will now try to install it...")
  install.packages("kernlab")
  library(kernlab)
}

# converts 'train' class of caret from S3 to S4
setOldClass("train")

#' Source files
source("middleman/FileManipulator.R")
source("classifier/GenericClassifier.R")
source("classifier/AnnClassifier.R")
source("classifier/BayesClassifier.R")
source("classifier/KnnClassifier.R")
source("classifier/TreeClassifier.R")
source("classifier/SvmClassifier.R")
source("preprocessor/FeatureEngineer.R")
source("preprocessor/InapRemover.R")
source("preprocessor/Normalizer.R")
source("preprocessor/DataCompressor.R")
source("mfExtractor/mf1Extractor.R")
source("mfExtractor/mf2Extractor.R")
source("optimizer/Optimizer.R")
source("evaluator/PerformanceEvaluator.R")
source("evaluator/HypothesisTester.R")
source("middleman/Expert.R")
source("classifier/Ensembler.R")
source("preprocessor/DataPrepare.R")
source("visualizer/FeatureVisualizer.R")
source("visualizer/PerformanceVisualizer.R")
source("middleman/Server.R")
source("middleman/InputParser.R")


#' Create workspace (if it doesn't exist already)
mainDir       <- getwd()
subDir        <- "workspace"
workspace_dir <- file.path(mainDir, subDir)
dir.create(workspace_dir, showWarnings = FALSE)