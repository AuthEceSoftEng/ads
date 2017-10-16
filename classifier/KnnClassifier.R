#' A class responsible for training a K-nearest neighbot classifier.
#'
#' KnnClassifier offers the functionalities of training a model and checking if training examples are enough based on the theory of VC-dimension. 
#' The type of model can be found in \code{"model_name_"}
#' 
#' @include GenericClassifier.R 
#' @include FileManipulator.R
#' 
#' @import methods
#' @import caret
#' @export
KnnClassifier <- setRefClass(Class = "KnnClassifier",
                             fields = list(),
                             contains = "GenericClassifier"
                             )

KnnClassifier$methods(
  #' Training of a K-nearest neighbor machine learning model
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
    'Train k-nearest neighbor classification models. The names of trained models are returned.'
    # transform parameters to useful form
    optParameters <- expand.grid( k = c(parameters$k))
    model_files <- list()
    for (row in 1:nrow(optParameters)) {
      current_parameters <- as.data.frame(optParameters[row,])
      colnames(current_parameters) <- colnames(optParameters)
      # train model
      set.seed(seed_)
      trained_model      <- suppressWarnings(train(Class ~ ., data = training_dataset,
                                                   method = model_name_,
                                                   tuneGrid = current_parameters,
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
  #' For a K-nearest neighbor classifier d_vc equals to 2 for k>3 and is infinite otherwise, according to
  #' \url{https://www.quora.com/How-does-the-parameter-k-affect-the-VC-dimension-of-the-k-nearest-neighbor-classifier}
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
    k        <- model$bestTune$k
    if(k < 3) {  
      d_vc <- Inf
    } else {
      d_vc <- 2
    }
    info     <- list(d_vc = d_vc, adequate = FALSE)
    if(N >= (10*d_vc)) {
      info$adequate <- TRUE
    }
    return(info)
  },
  initialize=function(...) {
    model_name_ <<- "knn"
    model_parameters_ <<- c("k")
    callSuper(...)
  }
)
