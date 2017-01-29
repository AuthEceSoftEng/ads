setwd("../..")

test_that("Test loadDataset",{
  file_manipulator <- FileManipulator$new(directories_ = list( Workspace = "workspace"))
  dataset <- file_manipulator$loadDataset("baboon_mating.csv")
  expect_that(dataset,  is_a("data.frame"))
})

test_that("Test loadResults",{
  file_manipulator <- FileManipulator$new(directories_ = list( Workspace = "workspace", Project = "workspace/project_my_first_experiment"))
  task <- list(compare = list(techniques = "models"))
  dataset <- file_manipulator$loadResults(task)
  expect_that(dataset,  is_a("list"))
})

test_that("Test clearModels",{
 file_manipulator <- FileManipulator$new(directories_ = list( Workspace = "workspace", Project = "workspace/project_my_first_experiment"))
 dataset <- file_manipulator$clearModels()
 models_path <- "workspace/project_my_first_experiment/model/model_files"
 expect_true(length(list.files(models_path)) == 0)
})


test_that("Test loadOrderedDictionary",{
 file_manipulator <- FileManipulator$new(directories_ = list( Workspace = "workspace", Project = "workspace/project_my_first_experiment"))
 dic <- file_manipulator$loadOrderedDictionary()
 expect_equal(names(dic), c("Attributes", "Values"))
})


test_that("Test saveModel",{
 file_manipulator <- FileManipulator$new(directories_ = list( Workspace = "workspace", Project = "workspace/project_my_first_experiment"))
 library(mlbench)
 data(Sonar)
 fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           repeats = 1)
  gbmFit1 <- train(Class ~ ., data = Sonar, 
                 method = "gbm", 
                 trControl = fitControl,
               verbose = FALSE
                 )
 model_file <- file_manipulator$saveModel(model = gbmFit1, model_name = "test_model")
 models_path <- "workspace/project_my_first_experiment/model/model_files"
 expect_true( model_file %in% list.files(models_path))

})

test_that("Test saveEnsemble",{
 file_manipulator <- FileManipulator$new(directories_ = list( Workspace = "workspace", Project = "workspace/project_my_first_experiment"))
 library(mlbench)
 data(Sonar)
 fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           repeats = 1)
  gbmFit1 <- train(Class ~ ., data = Sonar, 
                 method = "gbm", 
                 trControl = fitControl,
               verbose = FALSE
                 )
 test_info  <- list(dummy_variable = 0)
 model_file <- file_manipulator$saveEnsemble(included_models = list(gbmFit1), info = test_info)
 load("workspace/project_my_first_experiment/dummy_variable_info.RData")
 expect_equal(info, test_info)
})

test_that("Test saveDataset",{
  file_manipulator <- FileManipulator$new(directories_ = list( Workspace = "workspace", Project = "workspace/project_my_first_experiment"))
  path <- "data.csv"
  library(mlbench)
  data(Sonar)
  file_manipulator$saveCsv(dataset = Sonar, directory = path)
})

test_that("Test saveXml",{
 # not implemented
})

test_that("Test convertListToXmlTree",{
  # not implemented
})

test_that("Test saveRData",{
 file_manipulator <- FileManipulator$new(directories_ = list( Workspace = "workspace", Project = "workspace/project_my_first_experiment"))
 test_info  <- list(dummy_variable = 0)
 data_file <- file_manipulator$saveRData( data = test_info, file = "dummy.RData")
 load("workspace/dummy.RData")
 expect_equal(data, test_info)
})

test_that("Test savePng",{
 # not implemented
})

test_that("Test generateReport",{
  # not implemented
})
