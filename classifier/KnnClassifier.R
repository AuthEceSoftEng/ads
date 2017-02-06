##' A class responsible for training a k-nearest neighbor classifier
##' 
##' @include GenericClassifier.R 
##' @import methods
##' @export
KnnClassifier <- setRefClass(Class = "KnnClassifier",
                             fields = list(

                             ),
                             contains = "GenericClassifier"
)

KnnClassifier$methods(
  trainModel = function(training_dataset, parameters) {
    'Train a classification model.'
    parameters    <- parameters[[1]]
    optParameters <- expand.grid( k = c(parameters$k)
                                  )
    #train models
    models <- list()
    for (row in 1:nrow(optParameters)) {
      x<- optParameters[row,]
      example            <- matrix(x, nrow=1, ncol=ncol(optParameters), byrow=TRUE)
      example            <- data.frame(example)
      colnames(example)  <- names(optParameters)
      current_parameters <- example
      trained_model      <- suppressWarnings(train(Class ~ ., data = training_dataset,
                                              method = model_name_,
                                              tuneGrid = current_parameters,
                                              trControl=trainControl(method="none", classProbs =  TRUE))
      )
      colnames(trained_model$trainingData)[which(names(trained_model$trainingData) == ".outcome")] <- "Class"
      models[[row]]      <- trained_model
    }
    return(models)
  },
  initialize=function(...) {
    model_name_ <<- "knn"
    callSuper(...)
  }
)
