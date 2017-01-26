##' A class responsible for training a bayesian classifier
##' 
##' @include GenericClassifier.R 
##' @import methods
##' @export
BayesClassifier <- setRefClass(Class = "BayesClassifier",
                             fields = list(
                             ),
                             contains = "GenericClassifier"
)

BayesClassifier$methods(
  trainModel = function(training_dataset, parameters) {
    'Train a classification model.'
    library(e1071)
    optParameters <- expand.grid( fL = c(parameters$fL),
                                  usekernel = c(parameters$usekernel),
                                  adjust = c(parameters$adjust)
                                  )
    
    #train model
    trained_model <- suppressWarnings(train(Class ~ ., data = training_dataset,
                           method = model_name_, 
                           tuneGrid = optParameters,
                           trControl=trainControl(method="none", classProbs =  TRUE))
    )
    return(trained_model)
  },
  initialize=function(...) {
    model_name_ <<- "nb"
    callSuper(...)
  }
)
