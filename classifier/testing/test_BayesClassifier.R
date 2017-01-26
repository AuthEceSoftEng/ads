setwd("../..")
test_that("Test that trainModel returns model",{
  classifier = BayesClassifier$new()
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  test_dictionary <- file_manipulator$loadOrderedDictionary()
  test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  test_parameters <- list(fL = 2, usekernel = FALSE, adjust = 0.2)
  test_model <- classifier$trainModel( test_dataset, parameters = test_parameters)
    file_manipulator$saveModel(test_model, model_name = test_model$method)
  expect_that(test_model,  is_a("train"))
})
