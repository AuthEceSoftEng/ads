setwd("../..")

test_that("Test calculateConfusionMatrixMetrics",{
  project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  test_dictionary <- file_manipulator$loadOrderedDictionary()
  test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  # load test model (should be nnet_size_1_decay_1e-04_model.rds)
  class_attribute <- test_dataset$Class
  predictions     <- class_attribute
  #predictions     <- as.factor(sample(0:1, length(class_attribute), replace = TRUE))
  levels(predictions) <- c("Negative", "Positive")
  metrics <- c("Accuracy", "Recall", "Precision", "Fmeasure", "Matthew_coeff")
  performance_evaluator$setPredictions(predictions = predictions)
  performance_evaluator$setActualClass(actual_class = class_attribute)
  performance <- performance_evaluator$calculateConfusionMatrixMetrics(selected_metrics = metrics)
  expect_equal(performance, list(Accuracy = 1, Recall = 1,
                                               Precision = 1, Fmeasure = 1, Matthew_coeff = 1))
})

test_that("Test calculateAUC",{
  project_dir <- "workspace/project_my_first_experiment"
  performance_evaluator = PerformanceEvaluator$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  test_dictionary <- file_manipulator$loadOrderedDictionary()
  test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  # load test model (should be nnet_size_1_decay_1e-04_model.rds)
  class_attribute <- test_dataset$Class
  positive     <- runif(length(class_attribute), min=0, max=1)
  negative     <- runif(length(class_attribute), min=0, max=1)
  probabilities <- list()
  probabilities$Positive <- positive
  probabilities$Negative <- negative
  levels(probabilities) <- c("Negative", "Positive")
  performance_evaluator$setActualClass(actual_class = class_attribute)
  auc <- performance_evaluator$calculateAUC(predicted_probs = probabilities)
  expect_true(auc >= 0 && auc <= 1)
})
