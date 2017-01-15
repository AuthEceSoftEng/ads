#converts 'train' class of caret from S3 to S4
setOldClass("train")

##' A generic class, defining the functionality of a classifier
##'
##' @import methods
##' @export
GenericClassifier <- setRefClass(Class = "GenericClassifier",
                     fields = list(
                       dataset_ = "data.frame",
                       num_models_ = "numeric",
                       class_attribute_ = "data.frame",
                       accuracy_ = "numeric",
                       seed_ = "numeric"
                     ),
                     methods = list(
                       getNumModels = function() {
                         'Returns the number of models in directory model/model_files.'
                         num_models_ <<- length(getModels())
                         return(num_models_)
                       },
                       getModels = function() {
                         'Returns the models in directory model/model_files.'
                         # project_dir <- server_$getProjectDir()
                         project_dir <- "/home/elena/R_ws/ADS/ADS_workspace/project_temp"
                         model_files_directory <- "model/model_files"
                         model_files_directory <- file.path(project_dir, model_files_directory)
                         model_files <- list.files(model_files_directory, recursive = TRUE)
                         model_files <- paste(model_files_directory, model_files, sep = "/")
                         models <- lapply(model_files, readRDS)
                         return(models)
                       },
                       getDataset = function() {
                         'Returns the private dataset of Classifier.'
                         return(dataset_)
                       },
                       getAccuracy= function() {
                         'Returns the private dataset of Classifier.'
                         return(accuracy_)
                       },
                       getClass = function() {
                         'Returns the  the class vector.'
                         return(class_attribute_)
                       },
                       predictClassifier = function(model_to_pred, dataset) {
                         'Predicts using a classification model.'
                         model <- model_to_pred
                         predictions <- stats::predict(model, dataset, type = "prob")
                         return(predictions)
                       },
                       calculateAccuracy = function(model_to_acc) { 
                         'Calculate accuracy of classification model, provided as .rds entity.'
                         model_to_pred <- model_to_acc
                         probabilities <- predictClassifier(model_to_pred, dataset = dataset_)
                         predictions <- as.numeric(((probabilities))>0.5)
                         class_attribute <- as.numeric(class_attribute_[,1])
                         class_attribute[class_attribute==2] <-0
                         accuracy <- 1 - sum(abs(class_attribute - predictions)) /length(class_attribute)
                         return(accuracy)
                       },
                       setDataset = function(dataset) {
                         'Set private dataset of Classifier.'
                         dataset_ <<- dataset
                       },
                       setClassAttribute = function(class_attribute) {
                         'Set class attribute of private dataset.'
                         class_attribute_ <<- class_attribute
                       },
                       trainModel = function(training_dataset) {
                         'Train a classification model.'
                          cat("*** GenericClassifier: no effect ***")
                          return(NULL)
                       },
                       initialize=function(...) {
                         seed_ <<- 1
                         set.seed(seed_)
                         num_models_ <<- 0
                         accuracy_ <<- 0
                         callSuper(...)
                         .self
                       }
                     )
)
