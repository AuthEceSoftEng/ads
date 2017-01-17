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
    trainModel = function(training_dataset, parameters, project_dir) {
      'Train a classification model.'
      project_dir_ <<- project_dir
      optParameters <- expand.grid( size = c(parameters$size) ,
                                    decay = c(parameters$decay)
      )
      
      #train model
      trained_model <- train(Class ~ ., data = training_dataset,
                             method = "nnet", #Neural Network with parameters size and decay
                             tuneGrid = optParameters,
                             trControl=trainControl(method="none", classProbs =  TRUE)
      )
      #save model 
      #  project_dir <- server_$getProjectDir()      
      model_dir <- file.path(project_dir, "model/model_files")
      model_file <- file.path(model_dir, "ann_model.rds")
      #print(model_file)
      saveRDS(trained_model, model_file)
      return(trained_model)
    },
    initialize=function(...) {
      model_name_ <<- "nnet"
      callSuper(...)
    }
)
