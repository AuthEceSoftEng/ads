setwd("../..")
test_that("Test predictClassifier",{
  project_dir <- "workspace/project_my_first_experiment"
  classifier = GenericClassifier$new()
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  test_dictionary <- file_manipulator$loadOrderedDictionary()
  test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  # load test model (should be nnet_size_1_decay_1e-04_model.rds)
  test_model <- classifier$getModels(project_dir = project_dir)[[1]]
  predictions <- classifier$predictClassifier(model_to_pred = test_model, dataset = test_dataset, type = "raw")
  probabilities <- classifier$predictClassifier(model_to_pred = test_model, dataset = test_dataset, type = "prob")
  expect_equal(nlevels(as.factor(predictions)),  2)
  expect_equal(names(probabilities), c("Negative", "Positive"))
})
