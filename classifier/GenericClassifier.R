##' A generic class, defining the functionality of a classifier.
##'
##' @import methods
##' @export
GenericClassifier <- setRefClass(Class = "GenericClassifier",
                     fields = list(
                       dataset_         = "data.frame",
                       num_models_      = "numeric",
                       class_attribute_ = "data.frame",
                       accuracy_        = "numeric",
                       seed_            = "numeric",
                       model_name_      = "character",
                       model_parameters_ = "character"
                     ),
                     methods = list(
                       getNumModels = function(project_dir, ... ) {
                         'Returns the number of models in directory model/model_files.'
                         num_models_ <<- length(getModels(project_dir))
                         return(num_models_)
                       },
                       getModels = function(project_dir, ...) {
                         'Returns the models in directory model/model_files.'
                         model_files_directory <- "model/model_files"
                         model_files_directory <- file.path(project_dir, model_files_directory)
                         model_files           <- list.files(model_files_directory, recursive = TRUE)
                         model_files           <- file.path(model_files_directory, model_files)
                         return(model_files)
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
                       predictClassifier = function(model_to_pred, dataset, type = "raw") {
                         'Predicts using a classification model. Model_to_pred is the name of the file containing the model.'
                         # load model
                         load(model_to_pred)
                         predictions <- predict(model, dataset, type = type)
                         return(predictions)
                       },
                       setDataset = function(dataset) {
                         'Set private dataset of Classifier.'
                         dataset_ <<- dataset
                       },
                       setClassAttribute = function(class_attribute) {
                         'Set class attribute of private dataset.'
                         class_attribute_ <<- class_attribute
                       },
                       trainModel = function(training_dataset, parameters, project_dir, file_manipulator) {
                         'Train a classification model.'
                          cat("*** GenericClassifier: no effect ***")
                          return(NULL)
                       },
                       getModelName = function(...) {
                         'Returns name of algorithm used in model.'
                         return(model_name_)
                       },
                       getModelParameters = function(...) {
                         'Returns a vector of hyperparameters for particular model.'
                         return(model_parameters_)
                       },
                       initialize=function(...) {
                         seed_       <<- 1
                         num_models_ <<- 0
                         accuracy_   <<- 0
                         callSuper(...)
                         .self
                       }
                     )
)
