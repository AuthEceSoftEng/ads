setwd("../..")

test_that("Test get2MetaFeatures",{
  project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  test_dictionary <- file_manipulator$loadOrderedDictionary()
  test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  expert <- Expert$new()
  algorithm <- list(algorithm = "nnet")
  task <- list()
  task$algorithm <- algorithm
  preprocessed_dataset <- expert$choosePreprocessing(test_dataset, task)
  mf2_extractor <- mf2Extractor$new()
  metafeatures <- mf2_extractor$get2MetaFeatures(dataset = preprocessed_dataset)
  expect_equal(nrow(metafeatures),1)
  expect_equal(ncol(metafeatures),76)
  
})

test_that("Test calculate2MetaFeatures",{
  
})

