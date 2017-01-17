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
                       seed_ = "numeric",
                       project_dir_ = "character",
                       model_name_ = "character"
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
                         model_files_directory <- "model/model_files"
                         
                         model_files_directory <- file.path(project_dir_, model_files_directory)
                         model_files <- list.files(model_files_directory, recursive = TRUE)
                         model_files <- file.path(model_files_directory, model_files)
                         
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
                         predictions <- predict(model, dataset, type = "prob")
                         return(predictions)
                       },
                       calculateAccuracy = function(model_to_acc, test_dataset) { 
                         'Calculate accuracy of classification model, provided as .rds entity.'
                         #str(test_dataset)
                         #str(model_to_acc)
                         model_to_pred <- model_to_acc
                         class_attribute <- test_dataset$Class
                         test_dataset$Class <- NULL
                         probabilities <- predictClassifier(model_to_pred, dataset = test_dataset)
                         indexes <- which((probabilities$Negative>0.5))
                         predictions <- seq(1, nrow(probabilities))
                         predictions[indexes] <- 0
                         predictions[-indexes] <- 1
                         predictions <- factor(predictions, levels = c(0,1), labels = c("Negative","Positive"))
                         cm <- confusionMatrix(predictions, class_attribute)
                         accuracy <- as.numeric(cm$overall['Accuracy'])
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
                       trainModel = function(training_dataset, parameters, project_dir) {
                         'Train a classification model.'
                          cat("*** GenericClassifier: no effect ***")
                          return(NULL)
                       },
                       getModelName = function(...) {
                         'Returns name of algorithm used in model'
                         return(model_name_)
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
