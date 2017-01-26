setwd("../..")

test_that("Test that ensemble returns included models",{

  project_dir <- "workspace/project_my_first_experiment"
  # load and prepare test dataset
  test_dataset <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare <- DataPrepare$new()
  test_dictionary <- file_manipulator$loadOrderedDictionary()
  test_dataset <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  task <- list()
  # call ensembler$ensemble
  ensembler = Ensembler$new()
  test_classifier = GenericClassifier$new()
  ensemble_models <- ensembler$ensemble( classifier = test_classifier, test_dataset = test_dataset,
                                          performance_metric = "auc", project_dir = project_dir)
  expect_true(!is.null(ensemble_models))
  
})

test_that("Test that ensemble returns ensemble predictions of its models",{
  project_dir      <- "workspace/project_my_first_experiment"
  # load and prepare test dataset
  test_dataset     <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare     <- DataPrepare$new()
  test_dictionary  <- file_manipulator$loadOrderedDictionary()
  test_dataset     <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  task             <- list()
  # call ensembler$ensemble
  ensembler        <- Ensembler$new()
  test_classifier  <- GenericClassifier$new()
  test_models      <- test_classifier$getModels(project_dir = project_dir)
  test_datasets    <- list()
  for(i in 1:length(test_models)) {
    test_datasets[[i]] <- test_dataset
  }
  predictions      <- ensembler$getEnsemblePredictions( models = test_models, datasets = test_datasets, type = "raw")
  probabilities    <- ensembler$getEnsemblePredictions( models = test_models, datasets = test_datasets, type = "prob")
  expect_equal(nlevels(as.factor(predictions)),  2)
  expect_equal(names(probabilities), c("Negative", "Positive"))  
})

test_that("Test that updateEnsemble updates members of ensemble",{
  project_dir      <- "workspace/project_my_first_experiment"
  # load and prepare test dataset
  test_dataset     <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare     <- DataPrepare$new()
  test_dictionary  <- file_manipulator$loadOrderedDictionary()
  test_dataset     <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  # call ensembler$ensemble
  number_instances <- nrow(test_dataset)
  ensembler        <- Ensembler$new(test_dataset_= test_dataset, probabilities_ = data.frame( Negative = rep(0, number_instances), Positive =rep(0,number_instances), stringsAsFactors = FALSE),class_attribute_ = test_dataset$Class,
                                     performance_metric_ = "auc")
  test_classifier  <- GenericClassifier$new()
  test_models      <- test_classifier$getModels(project_dir = project_dir)
  test_model       <- test_models[[1]]

  ensemble_models  <- ensembler$updateEnsemble(model = test_model)
  performance      <- ensembler$getPerformance()
  num_models       <- ensembler$getNumModels()
  inc_models       <- ensembler$getIncludedModels()
  expect_equal(num_models, 1)
  expect_equal(length(inc_models), 1)
  expect_true(performance >0)
  expect_that(inc_models[[1]],  is_a("train"))
  
  
})

test_that("Test that evaluateModelContribution returns model contributions",{
  project_dir      <- "workspace/project_my_first_experiment"
  # load and prepare test dataset
  test_dataset     <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare     <- DataPrepare$new()
  test_dictionary  <- file_manipulator$loadOrderedDictionary()
  test_dataset     <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  # call ensembler$ensemble
  ensembler        <- Ensembler$new(test_dataset_= test_dataset, class_attribute_ = test_dataset$Class,
                                     performance_metric_ = "auc")
  test_classifier  <- GenericClassifier$new()
  test_models      <- test_classifier$getModels(project_dir = project_dir)
  contributions    <- ensembler$evaluateModelContribution(models_to_eval = test_models)
  expect_true(length(contributions) == length(test_models))
})


test_that("Test that initializeEnsemble initializes the ensemble with N best ranking models",{
  project_dir      <- "workspace/project_my_first_experiment"
  # load and prepare test dataset
  test_dataset     <- read.csv("workspace/datasets_repo/baboon_mating.csv",
                           header = TRUE, sep=",", stringsAsFactors=FALSE)
  file_manipulator <- FileManipulator$new()
  data_prepare     <- DataPrepare$new()
  test_dictionary  <- file_manipulator$loadOrderedDictionary()
  test_dataset     <- data_prepare$convertAttributeTypes(dataset = test_dataset, dictionary = test_dictionary)
  # call ensembler$ensemble
  number_instances <- nrow(test_dataset)
  ensembler        <- Ensembler$new(test_dataset_= test_dataset, class_attribute_ = test_dataset$Class,
                                     performance_metric_ = "auc", probabilities_ = data.frame( Negative = rep(0, number_instances), Positive =rep(0,number_instances), stringsAsFactors = FALSE), perc_initial_ = 0.4)
  ensembler$initializeEnsemble(project_dir = project_dir)
  included_models  <- ensembler$getIncludedModels()
  expect_true( length(included_models) == 2)
  
})

