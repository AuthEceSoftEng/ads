setwd("../..")

test_that("Test optimizeHParam",{
  metafeatures_file <- "/home/elena/R_ws/automl-software/workspace/HPP/metafeatures/KnnClassifier/autosklearn_metafeatures.csv"
  metafeatures <- read.csv(metafeatures_file, header = TRUE, sep=",", stringsAsFactors=FALSE)
  test_metafeature <- metafeatures[c(12),]
  test_metafeature[is.na(test_metafeature)] <- 0
  test_class <- test_metafeature$Class
  test_metafeature$Class <- NULL
  test_file_manipulator <- FileManipulator$new(directories_ = list( Workspace = "workspace", Project = "workspace/project_my_first_experiment") )
  test_optimizer <- Optimizer$new(file_manipulator_ = test_file_manipulator)
  predictions_list <- test_optimizer$optimizeHParam(algorithm = "KnnClassifier", metafeatures = test_metafeature)
  str(predictions_list)
})

test_that("Test getPredictionIntervals ",{
  metafeatures_file <- "/home/elena/R_ws/automl-software/workspace/HPP/metafeatures/KnnClassifier/autosklearn_metafeatures.csv"
  metafeatures <- read.csv(metafeatures_file, header = TRUE, sep=",", stringsAsFactors=FALSE)
  test_metafeature <- metafeatures[c(4),]
  test_metafeature[is.na(test_metafeature)] <- 0
  test_class <- test_metafeature$Class
  test_metafeature$Class <- NULL
  test_file_manipulator <- FileManipulator$new(directories_ = list( Workspace = "workspace", Project = "workspace/project_my_first_experiment") )
  test_optimizer <- Optimizer$new(file_manipulator_ = test_file_manipulator, algorithm_ = "KnnClassifier" )
  test_predictions <- test_optimizer$getModelPredictions(algorithm = "KnnClassifier", metafeatures = test_metafeature)
  prediction_intervals     <- test_optimizer$getPredictionIntervals(parameters = test_predictions, algorithm = "KnnClassifier", metafeatures =test_metafeature)
  #str(prediction_intervals)
})


test_that("Test getModelPredictions",{
  metafeatures_file <- "/home/elena/R_ws/automl-software/workspace/HPP/metafeatures/KnnClassifier/autosklearn_metafeatures.csv"
  metafeatures <- read.csv(metafeatures_file, header = TRUE, sep=",", stringsAsFactors=FALSE)
  test_metafeature <- metafeatures[c(4),]
  test_metafeature[is.na(test_metafeature)] <- 0
  test_class <- test_metafeature$Class
  test_metafeature$Class <- NULL
  test_file_manipulator <- FileManipulator$new(directories_ = list( Workspace = "workspace", Project = "workspace/project_my_first_experiment") )
  test_optimizer <- Optimizer$new(file_manipulator_ = test_file_manipulator, algorithm_ = "KnnClassifier")
  test_predictions <- test_optimizer$getModelPredictions(algorithm = "KnnClassifier", metafeatures = test_metafeature)
  #str(test_predictions)
})


