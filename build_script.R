
#' A script that sets up automl's workspace. It installs missing packages and sources the files.
#' Run with `source("build_script.R")` from automl directory. <br> <br> <br>

#' Install and load missing  packages
library_path <- "lib"
.libPaths(c( .libPaths(),library_path))
dir.create(library_path, showWarnings = FALSE)
repos_path <- "http://cran.rstudio.com/"

if(!require(doParallel))
{
  print("You are missing the package 'doParallel', we will now try to install it...")
  install.packages("doParallel", repos = repos_path, lib = library_path)
  library(doParallel)
}
if(!require(foreach))
{
  print("You are missing the package 'foreach', we will now try to install it...")
  install.packages("foreach", repos = repos_path, lib = library_path)
  library(foreach)
}
if(!require(PMCMR))
{
  print("You are missing the package 'PMCMR', we will now try to install it...")
  install.packages("PMCMR", repos = repos_path, lib = library_path)
  library(PMCMR)
}
if(!require(reshape2))
{
  print("You are missing the package 'reshape2', we will now try to install it...")
  install.packages("reshape2", repos = repos_path, lib = library_path)
  library(reshape2)
}
if(!require(nnet))
{
  print("You are missing the package 'nnet', we will now try to install it...")
  install.packages("nnet", repos = repos_path, lib = library_path)
  library(nnet)
}
if(!require(ROCR))
{
  print("You are missing the package 'ROCR', we will now try to install it...")
  install.packages("ROCR", repos = repos_path, lib = library_path)
  library(ROCR)
}
if(!require(plot3D))
{
  print("You are missing the package 'plot3D', we will now try to install it...")
  install.packages("plot3D", repos = repos_path, lib = library_path)
  library(plot3D)
}
if(!require(akima))
{
  print("You are missing the package 'akima', we will now try to install it...")
  install.packages("akima", repos = repos_path, lib = library_path)
  library(akima)
}
if(!require(fields))
{
  print("You are missing the package 'fields', we will now try to install it...")
  install.packages("fields", repos = repos_path, lib = library_path)
  library(fields)
}
if(!require(ggplot2))
{
  print("You are missing the package 'ggpllot2', we will now try to install it...")
  install.packages("ggplot2", repos = repos_path, lib = library_path)
  library(ggplot2)
}
if(!require(plyr))
{
  print("You are missing the package 'plyr', we will now try to install it...")
  install.packages("plyr", repos = repos_path, lib = library_path)
  library(plyr)
}
if(!require(ranger))
{
  print("You are missing the package 'ranger', we will now try to install it...")
  install.packages("ranger", repos = repos_path, lib = library_path)
  library(ranger)
}
if(!require(optparse))
{
  print("You are missing the package 'optparse', we will now try to install it...")
  install.packages("optparse", repos = repos_path, lib = library_path)
  library(optparse)
}
if(!require(randomForest))
{
  print("You are missing the package 'randomForest', we will now try to install it...")
  install.packages("randomForest", repos = repos_path, lib = library_path)
  library(randomForest)
}
if(!require(doParallel))
{
  print("You are missing the package 'doParallel', we will now try to install it...")
  install.packages("doParallel", repos = repos_path , lib = library_path)
  library(doParallel)
}
if(!require(foreach))
{
  print("You are missing the package 'foreach', we will now try to install it...")
  install.packages("foreach", repos = repos_path, lib = library_path)
  library(foreach)
}
if(!require(methods))
{
  print("You are missing the package 'methods', we will now try to install it...")
  install.packages("methods", repos = repos_path, lib = library_path)
  library(methods)
}
if(!require(caret))
{
  print("You are missing the package 'caret', we will now try to install it...")
  install.packages("caret", dependencies = TRUE, repos = repos_path, lib = library_path)
  library(caret)
}
if(!require(FactoMineR))
{
  print("You are missing the package 'FactoMineR', we will now try to install it...")
  install.packages("FactoMineR", repos = repos_path, lib = library_path)
  library(FactoMineR)
}
if(!require(MASS))
{
  print("You are missing the package 'MASS', we will now try to install it...")
  install.packages("MASS", repos = repos_path, lib = library_path)
  library(MASS)
}
if(!require(tikzDevice))
{
  print("You are missing the package 'tikzDevice', we will now try to install it...")
  install.packages("tikzDevice", repos = repos_path, lib = library_path)
  library(tikzDevice)
}
if(!require(coin))
{
  print("You are missing the package 'coin', we will now try to install it...")
  install.packages("coin", repos = repos_path, lib = library_path)
  library(coin)
}
if(!require(multcomp))
{
  print("You are missing the package 'multcomp', we will now try to install it...")
  install.packages("multcomp", repos = repos_path, lib = library_path)
  library(multcomp)
}
if(!require(colorspace))
{
  print("You are missing the package 'colorspace', we will now try to install it...")
  install.packages("colorspace", repos = repos_path, lib ="/home/elennisioti/thesis_ws/lib")
  library(colorspace)
}
if(!require(pROC))
{
  print("You are missing the package 'pROC', we will now try to install it...")
  install.packages("pROC", repos = repos_path, lib = library_path)
  library(pROC)
}
if(!require(e1071))
{
  print("You are missing the package 'e1071', we will now try to install it...")
  install.packages("e1071", repos = repos_path, lib = library_path)
  library(e1071)
}
if(!require(kernlab))
{
  print("You are missing the package 'kernlab', we will now try to install it...")
  install.packages("kernlab", repos = repos_path, lib = library_path)
  library(kernlab)
}
if(!require(knitr))
{
  print("You are missing the package 'knitr', we will now try to install it...")
  install.packages("knitr", repos = repos_path, lib = library_path)
  library(knitr)
}
if(!require(markdown))
{
  print("You are missing the package 'markdown', we will now try to install it...")
  install.packages("markdown", repos = repos_path, lib = library_path)
  library(markdown)
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

#' Define workspace directory  
mainDir       <- getwd()
subDir        <- "workspace"
workspace_dir <- file.path(mainDir, subDir)


