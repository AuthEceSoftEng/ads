#'  A generic class, defining the functionality of a classifier.
#'
#' GenericClassifier offers the functionality of predicting using a trained machine learning models.
#' It also offers access to the repository of machine learning models and private members of the classes
#' \code{\ink{AnnClassifier}}, \code{\ink{BayesClassifier}},\code{\ink{KnnClassifier}} and \code{\ink{SvmClassifier}}.
#' 
#' @slot seed_ seed for control of randomness
#' @slot mode_name_ name of machine learning model
#' @slot model_parameters_ hyperparameters of algorithm 
#' @slot info_ list of information about a classifier
#' 
#' @include GenericClassifier.R 
#' @include FileManipulator.R
#' 
#' @import methods
#' @import caret
#' @export
GenericClassifier <- setRefClass(Class = "GenericClassifier",
                                 fields = list(
                                   seed_             = "numeric",
                                   model_name_       = "character",
                                   model_parameters_ = "character",
                                   info_             = "list"
                                   )
                                 )
                                 
GenericClassifier$methods(
  #' Training of a machine learning model
  #' 
  #' Generic interface to training a model
  #' 
  #' @name trainModel
  #' @alias trainModel
  #' 
  #' @param training_dataset dataset containing training instances
  #' @param parameters hyperparameters of algorithm
  #' @param file_manipulator an instance of Class \code{\link{FileManipulator}}.
  #' 
  #' @return NULL
  trainModel = function(training_dataset, parameters, project_dir, file_manipulator) {
    'Train a classification model.'
     cat("*** GenericClassifier: no effect ***")
     return(NULL)
  },
  #' Predict using a trained machine learning model.
  #' 
  #' Generic interface to training a model
  #' 
  #' @name predictModel
  #' @alias predictModel
  #' 
  #' @param model_to_pred model to use for prediction
  #' @param dataset dataset containing instances to predict
  #' @param type "raw" or "prob"
  #' 
  #' @return NULL
  predictModel = function(model_to_pred, dataset, type = "raw") {
    'Predicts using a classification model. Model_to_pred is the name of the file containing the model.'
     # load model
     load(model_to_pred)
     predictions <- predict(model, dataset, type = type)
     return(predictions)
  },
  #' Checking for adequateness of training examples
  #' 
  #' Generic interface that indicates that no calculating technique is available for a model 
  #' that hasn't defined its own implementation of this function.
  #' 
  #' @name AdequateExamples
  #' @alias AdequateExamples
  #' 
  #' @param model the trained model
  #' 
  #' @return NA
  AdequateExamples = function(model, ...) {
    'Denotes that the number of adequate examples is unknown. Used for algorithms with uncomputable VC-dimension.'
     return(NA)
  },
  #' Returning number of trained models
  #' 
  #' @name getNumModels
  #' @alias getNumModels
  #' 
  #' @param project_dir directory of current project
  #' 
  #' @return number of models
  getNumModels = function(project_dir, current_fold,  ... ) {
    'Returns the number of models in directory model/model_files.'
    return(length(getModels(project_dir, current_fold)))
  },
  #' Returning trained models
  #' 
  #' @name getModels
  #' @alias getModels
  #' 
  #' @param project_dir directory of current project
  #' 
  #' @return a list of trained model files
  getModels = function(project_dir, current_fold, ...) {
    'Returns the models in directory model/model_files.'
    model_files_directory <- "model/model_files"
    model_files_directory <- file.path(project_dir, model_files_directory, current_fold)
    model_files           <- list.files(model_files_directory)
    model_files           <- file.path(model_files_directory, model_files)
    return(model_files)
  },
  #' Return name of model
  #' 
  #' Caret's names for method is returned.
  #' 
  #' @name getModelName
  #' @alias getModelName
  #' 
  #' 
  #' @return name of model
  getModelName = function(...) {
    'Returns name of algorithm used in model.'
     return(model_name_)
   },
  #' Return hyperparameters of model
  #' 
  #' The hyperparameters exposed by caret for current model are returned.
  #' 
  #' @name getModelParameters
  #' @alias getModelParameters
  #' 
  #' 
  #' @return vector of hyperparameters
  getModelParameters = function(...) {
    'Returns a vector of hyperparameters for particular model.'
     return(model_parameters_)
  },
  #' Return information about the classifier
  #' 
  #' Information includes adequateness of training instances.
  #' 
  #' @name getInfo
  #' @alias getInfo
  #' 
  #' 
  #' @return list of information
  getInfo = function(...) {
    'Returns information about classifier.'
    return(info_)
  },
  initialize=function(...) {
    seed_       <<- 1
    info_       <<- list()
    callSuper(...)
    .self
    }
)
