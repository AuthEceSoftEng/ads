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
    optParameters <- expand.grid( k = c(parameters$k)
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
    model_name_ <<- "knn"
    callSuper(...)
  }
)
