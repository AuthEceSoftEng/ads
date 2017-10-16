#' A class responsible for training an Artificial Neural Network.
#'
#' AnnClassifier offers the functionalities of training a model and checking if training examples are enough based on the theory of VC-dimension. 
#' The type of model can be found in \code{"model_name_"}
#' 
#' @include GenericClassifier.R 
#' @include FileManipulator.R
#' 
#' @import methods
#' @import caret
#' @export
AnnClassifier <- setRefClass(Class = "AnnClassifier",
                             contains = "GenericClassifier",
                             fields = list()
                             )

AnnClassifier$methods(
  #' Training of an Artificial Neural Network machine learning model
  #' 
  #' A model of type \code{"model_name"} is trained and stored. The adequateness of training examples is checked and the names of the
  #' files containing saved models are returned. 
  #' 
  #' @name trainModel
  #' @alias trainModel
  #' 
  #' @param training_dataset dataset containing training instances
  #' @param parameters hyperparameters of algorithm
  #' @param file_manipulator an instance of Class \code{\link{FileManipulator}}.
  #' 
  #' @return names of files containing saved models
  trainModel = function(training_dataset, parameters, file_manipulator, current_fold) {
    'Train ANN classification models. The names of trained models are returned.'
    # transform parameters to useful form
    optParameters <- expand.grid( size = c(parameters$size),
                                  decay = c(parameters$decay)
                                )
    model_files <- list()
    for (row in 1:nrow(optParameters)) {
      current_parameters <- as.data.frame(optParameters[row,])
      colnames(current_parameters) <- colnames(optParameters)
      # train model
      set.seed(seed_)
      trained_model      <- suppressWarnings(train(Class ~ ., data = training_dataset,
                                                  method = model_name_,
                                                  tuneGrid = current_parameters,
                                                  MaxNWts = 99999999 , # this parameter is used by nnet to
                                                  #prohibit large training times. We set it to an arbitrarily high value.
                                                  trControl=trainControl(method="none", classProbs =  TRUE))
                                            )
      # check if examples are adequate for training a model of this family
      if(row == nrow(optParameters)/2) { # checking is performed for the hyperparameter in the mid of the range (as considered more representative)
        info_ <<- AdequateExamples(model = trained_model)
      }
      colnames(trained_model$trainingData)[which(names(trained_model$trainingData) == ".outcome")] <- "Class"
      # save model
      model_file <- file_manipulator$saveModel(model = trained_model, model_name = model_name_, current_fold)
      # keep name of file
      model_files[[row]] <- model_file
    }
    return(model_files)
  },
  #' Checking for adequateness of training examples
  #' 
  #' The heuristic N >= d_vc is used, where N is the number of training examples and d_vc the VC dimension ot the algorithm.
  #' For an ANN d_vc equals to d+1, where d the number of weights used.  
  #' 
  #' @name AdequateExamples
  #' @alias AdequateExamples
  #' 
  #' @param model the trained model
  #' 
  #' @return a list containing the value of the VC-dimension and a logical indicating the adequateness of examples.
  AdequateExamples = function(model, ...) {
      'Returns a list object indicating VC-dimension and adequateness of examples.'
      N        <- nrow(model$trainingData)
      d_vc     <- length(coef(model$finalModel)) + 1
      info     <- list(d_vc = d_vc, adequate = FALSE)
      if(N>= (10*d_vc)) { # heuristic
        info$adequate <- TRUE
      }
      return(info)
    },
    initialize=function(...) {
      model_name_       <<- "nnet"
      model_parameters_ <<- c("size", "decay")
      callSuper(...)
    }
)
