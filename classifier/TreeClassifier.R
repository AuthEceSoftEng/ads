##' A class responsible for training a tree classifier
##' 
##' @include GenericClassifier.R 
##' @import methods
##' @export
TreeClassifier <- setRefClass(Class = "TreeClassifier",
                             fields = list(

                             ),
                             contains = "GenericClassifier"
)

TreeClassifier$methods(
  trainModel = function(training_dataset, parameters) {
    'Train a classification model.'
    optParameters <- expand.grid( maxdepth = c(parameters$maxdepth),
			           nu = c(parameters$nu),
                                  iter = c(parameters$iter)
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
    model_name_ <<- "ada"
    callSuper(...)
  }
)
