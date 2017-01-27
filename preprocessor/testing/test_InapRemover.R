setwd("../..")

test_that("Test removeUnknown",{ 
  project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  test_dictionary <- file_manipulator$loadOrderedDictionary()
  test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  inap_remover <- InapRemover$new()
  dataset <- inap_remover$removeUnknown(dataset = test_dataset)
  str(dataset)
  str(dataset[is.na(dataset)])
  # check no NAs
  expect_true(length(dataset[is.na(dataset)]) ==0)
   
})

test_that("Test removeInfinites",{ 
  project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  test_dictionary <- file_manipulator$loadOrderedDictionary()
  test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  inap_remover <- InapRemover$new()
  dataset <- inap_remover$removeInfinites(dataset = test_dataset)
  # check no Infs or -Infs
  expect_true((length(dataset[dataset == Inf]) == 0) & (length(dataset[dataset == -Inf]) == 0))
})

test_that("Test removeOutliers",{ 
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
  inap_remover <- InapRemover$new()
  dataset <- inap_remover$removeOutliers(dataset = num_dataset)
  # how to check that outliers are removed??

})

