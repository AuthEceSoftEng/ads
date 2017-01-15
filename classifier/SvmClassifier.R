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
      str(optParameters)
      str(training_dataset)
      #train model
      trained_model <- train(Class ~ ., data = training_dataset,
                             method = "svmRadial", #Neural Network with parameters size and decay
                             tuneGrid = optParameters,
                             trControl=trainControl(method="none")
      )
      #save model 
      model_dir <- file.path(project_dir, "model/model_files")
      model_file <- file.path(model_dir, "svm_model.rds")
      print(model_file)
      saveRDS(trained_model, model_file)
      return(trained_model)
    },
    initialize=function(...) {
      callSuper(...)
    }
)
