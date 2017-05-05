##' A class responsible for training an Artificial Neural Network.
##'
##' @include GenericClassifier.R 
##' @import methods
##' @export
AnnClassifier <- setRefClass(Class = "AnnClassifier",
                             fields = list(),
                             contains = "GenericClassifier"
                             )

AnnClassifier$methods(
    trainModel = function(training_dataset, parameters, file_manipulator) {
      'Train ANN classification models. The names of trained models are returned.'
      # define parameters
      parameters    <- parameters[[1]]
      optParameters <- expand.grid( size = c(parameters$size) ,
                                    decay = c(parameters$decay)
                                    )
      model_files <- list()
      for (row in 1:nrow(optParameters)) {
        current_parameters <- as.data.frame(lapply(optParameters, function(x) x[row]))
        # train model
        trained_model      <- suppressWarnings(train(Class ~ ., data = training_dataset,
                                                     method = model_name_,
                                                     tuneGrid = current_parameters,
                                                     MaxNWts = 99999999 , # this parameter is used by nnet to prohibit large training times
                                                     trControl=trainControl(method="none", classProbs =  TRUE))
                                               )
        colnames(trained_model$trainingData)[which(names(trained_model$trainingData) == ".outcome")] <- "Class"
        model_file <- file_manipulator$saveModel(model = trained_model, model_name = model_name_)
        # keep name of file
        model_files[[row]] <- model_file
      }
      return(model_files)
    },
    initialize=function(...) {
      model_name_ <<- "nnet"
      model_parameters_ <<- c("size", "decay")
      callSuper(...)
    }
)
