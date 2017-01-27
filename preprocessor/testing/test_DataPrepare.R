setwd("../..")

test_that("Test convertAttributeTypes",{ 
  project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  test_dictionary <- file_manipulator$loadOrderedDictionary()
  # add dummy ordered attributes
  test_dataset$height <- as.character(sample(0:1, nrow(test_dataset), replace = TRUE))
  test_dataset$dummy_ordered <- rep("high", nrow(test_dataset)) 
  test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  # test that characters are converted to factors
  expect_true( !all(sapply(test_dataset,class) == "character"))
  # test that ordinal factors are recognized
  expect_true(is.ordered(test_dataset$height))
  expect_true(is.ordered(test_dataset$dummy_ordered))
})

test_that("Test disposeRareLevels",{
  project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  variables    <- names(test_dataset[sapply(test_dataset,class) == "character"])
  test_dataset[, (names(test_dataset) %in% variables)] <- lapply(test_dataset[, (names(test_dataset) %in% variables)], as.factor)
  factor_dataset <- test_dataset[, (names(test_dataset) %in% variables)]
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  factor_dataset <- data_prepare$disposeRareLevels(dataset = factor_dataset)
  factor_threshold <- data_prepare$getFactorThreshold()
  # not sure what kind of test to apply  
  #str(as.vector(lapply(factor_dataset, function(x) nlevels(x))) < 49)
})

test_that("Test partitionData ",{
  project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  variables    <- names(test_dataset[sapply(test_dataset,class) == "character"])
  test_dataset[, (names(test_dataset) %in% variables)] <- lapply(test_dataset[, (names(test_dataset) %in% variables)], as.factor)
  factor_dataset <- test_dataset[, (names(test_dataset) %in% variables)]
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  partition1 <- data_prepare$partitionData(test_dataset, technique = list(name = "holdout", ratio = 0.7))
  partition2 <- data_prepare$partitionData(test_dataset, technique = list(name = "kfold", ratio = 0.8))
  partition3 <- data_prepare$partitionData(test_dataset, technique = list(name = "loocv"))
  # check holdout size
  expect_equal(length(partition1), ceiling(0.7 * nrow(test_dataset)))
  # check kfold size
  nr_folds <- 1/(1-0.8)
  expect_equal(ncol(partition2), nr_folds)
  expect_equal(nrow(partition2), ceiling(0.8 * nrow(test_dataset)))
  # check loocv size
  expect_equal(nrow(partition3), nrow(test_dataset)-1)
  expect_equal(ncol(partition3), nrow(test_dataset))
})


