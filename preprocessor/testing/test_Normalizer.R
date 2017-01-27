setwd("../..")

test_that("Test zscoreNormalize",{ 
 project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
    test_dictionary <- file_manipulator$loadOrderedDictionary()

    test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
   normalizer  <- Normalizer$new()
  normalizer$zscoreNormalize(dataset = test_dataset)
  # find appropriate tests
   
})

test_that("Test minMaxNormalize",{ 
   project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
    test_dictionary <- file_manipulator$loadOrderedDictionary()

    test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
   normalizer  <- Normalizer$new()
  normalizer$minMaxNormalize(dataset = test_dataset, range = c("min", "max"))
   # find appropriate tests
})


