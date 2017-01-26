##' A class responsible for training an svm
##'
##' @include GenericClassifier.R 
##' @import methods
##' @export
SvmClassifier <- setRefClass(Class = "SvmClassifier",
                                 fields = list(
                                 ),
                                   contains = "GenericClassifier"
                             )

SvmClassifier$methods(
    trainModel = function(training_dataset, parameters, project_dir) {
      'Train a classification model.'
      library(caret)
      library(kernlab)
      optParameters <- expand.grid( C = c(parameters$C) ,
                                    sigma = c(parameters$sigma)
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
      model_name_ <<- "svmRadial"
      callSuper(...)
    }
)
