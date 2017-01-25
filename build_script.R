#setwd("automl-software")

# Loading needed packages
if(!require(caret))
{
  print("You are missing the package 'caret', we will now try to install it...")
  install.packages("caret")
  library(coin)
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
  print("You are missing the package 'colorspace', we will now try to install it...")
  install.packages("colorspace")
  library(pROC)
}

if(!require(XML))
{
  print("You are missing the package 'colorspace', we will now try to install it...")
  install.packages("XML")
  library(XML)
}
source("classifier/GenericClassifier.R")
source("classifier/AnnClassifier.R")
source("classifier/BayesClassifier.R")
source("classifier/KnnClassifier.R")
source("classifier/TreeClassifier.R")
source("classifier/SvmClassifier.R")
source("mfExtractor/mf1Extractor.R")
source("mfExtractor/mf2Extractor.R")
source("optimizer/Optimizer.R")
source("middleman/FileManipulator.R")
source("preprocessor/FeatureEngineer.R")
source("preprocessor/InapRemover.R")
source("preprocessor/Normalizer.R")
source("preprocessor/DataCompressor.R")
source("evaluator/PerformanceEvaluator.R")
source("evaluator/HypothesisTester.R")
source("middleman/Expert.R")
source("classifier/Ensembler.R")
source("preprocessor/DataPrepare.R")
source("visualizer/FeatureVisualizer.R")
source("visualizer/PerformanceVisualizer.R")
source("middleman/Server.R")
source("middleman/InputParser.R")
source("optimizer/BayesOptimizer.R")
