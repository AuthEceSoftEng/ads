setwd("../..")

test_that("Test findOptimalBoxCoxTransform",{ 
  project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
    test_dictionary <- file_manipulator$loadOrderedDictionary()

    test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
    variables    <- names(test_dataset[sapply(test_dataset,class) == "factor"])
  test_dataset[, (names(test_dataset) %in% variables)] <- lapply(test_dataset[, (names(test_dataset) %in% variables)], as.factor)
  num_dataset <- test_dataset[, !(names(test_dataset) %in% variables)]
  str(num_dataset)
  num_dataset$Class <- test_dataset$Class
  feature_engineer <- FeatureEngineer$new()
  lambda <- feature_engineer$findOptimalBoxCoxTransform(train_dataset = num_dataset)
  str(lambda)
})

test_that("Test applyLogTransform",{
 project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
    test_dictionary <- file_manipulator$loadOrderedDictionary()

    test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
    variables    <- names(test_dataset[sapply(test_dataset,class) == "factor"])
  test_dataset[, (names(test_dataset) %in% variables)] <- lapply(test_dataset[, (names(test_dataset) %in% variables)], as.factor)
  num_dataset <- test_dataset[, !(names(test_dataset) %in% variables)]
  str(num_dataset)

  feature_engineer <- FeatureEngineer$new()
  transformed <- feature_engineer$applyLogTransform(dataset = num_dataset)
  str(transformed)
})

test_that("Test findCountFeatures ",{
   project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
    test_dictionary <- file_manipulator$loadOrderedDictionary()

    test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  

  feature_engineer <- FeatureEngineer$new()
  transformed <- feature_engineer$findCountFeatures(dataset = test_dataset)
str(transformed)
})


