#' A class responsible for training a tree classifier
#' 
#' TreeClassifier offers the functionalities of training a model and checking if training examples are enough based on the theory of VC-dimension. 
#' The type of model can be found in \code{"model_name_"}
#' 
#' @include GenericClassifier.R 
#' @import methods
#' @export
TreeClassifier <- setRefClass(Class = "TreeClassifier",
                             fields = list(

                             ),
                             contains = "GenericClassifier"
)

TreeClassifier$methods(
  trainModel = function(training_dataset, parameters, file_manipulator, current_fold) {
    'Train tree classification models. The names of trained models are returned.'
    # define parameters
    optParameters <- expand.grid( cp = c(parameters$cp))
    model_files <- list()
    for (row in 1:nrow(optParameters)) {
      current_parameters <- as.data.frame(optParameters[row,])
      colnames(current_parameters) <- colnames(optParameters)
      # train model
      trained_model      <- suppressWarnings(train(Class ~ ., data = training_dataset,
                                                   method = model_name_,
                                                   tuneGrid = current_parameters,
                                                   trControl=trainControl(method="none", classProbs =  TRUE))
      )
      colnames(trained_model$trainingData)[which(names(trained_model$trainingData) == ".outcome")] <- "Class"
      # save model
      model_file <- file_manipulator$saveModel(model = trained_model, model_name = model_name_, current_fold)
      # keep name of file
      model_files[[row]] <- model_file
    }
    return(model_files)
  },
  initialize=function(...) {
    model_name_ <<- "rpart"
    model_parameters_ <<- c("cp")
    callSuper(...)
  }
)
