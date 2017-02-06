setwd("../..")

test_that("Test performPCA",{
  project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  test_dictionary <- file_manipulator$loadOrderedDictionary()
  test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  data_compressor <- DataCompressor$new()
  compressed_dataset <- data_compressor$performPCA(dataset = test_dataset, variance = 1)
  str(compressed_dataset)
  expect_true(ncol(compressed_dataset) <= ncol(test_dataset))
})

test_that("Test performMDA",{
  project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  test_dictionary <- file_manipulator$loadOrderedDictionary()
  test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  data_compressor <- DataCompressor$new()
  compressed_dataset <- data_compressor$performMDA(dataset = test_dataset)
  expect_true(ncol(compressed_dataset) <= ncol(test_dataset))
})

