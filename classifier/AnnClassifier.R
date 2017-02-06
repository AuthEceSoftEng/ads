##' A class responsible for training an Artificial Neural Network
##'
##' @include GenericClassifier.R 
##' @import methods
##' @export
AnnClassifier <- setRefClass(Class = "AnnClassifier",
                                 fields = list(
                                 ),
                                   contains = "GenericClassifier"
                             )

AnnClassifier$methods(
    trainModel = function(training_dataset, parameters) {
      'Train a classification model.'
      optParameters <- expand.grid( size = c(parameters$size) ,
                                    decay = c(parameters$decay)
      )
      #train model
      trained_model <- suppressWarnings(train(Class ~ ., data = training_dataset,
                             method = model_name_, #Neural Network with parameters size and decay
                             tuneGrid = optParameters,
                             trControl=trainControl(method="none", classProbs =  TRUE))
      )
      return(trained_model)
    },
    initialize=function(...) {
      model_name_ <<- "nnet"
      callSuper(...)
    }
)
