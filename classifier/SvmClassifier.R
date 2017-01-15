##' A class responsible for training an svm
##'
##' @include GenericClassifier.R 
##' @import methods
##' @export
SvmClassifier <- setRefClass(Class = "SvmClassifier",
                                 fields = list(
                                   C_ = "numeric",
                                   Sigma_ = "numeric"
                                 ),
                                   contains = "GenericClassifier"
                             )

AnnClassifier$methods(
    trainModel = function(training_dataset) {
      'Train a classification model.'
      optParameters <- expand.grid( C = c(C_) ,
                                    sigma = c(Sigma_)
      )
      
      #train model
      trained_model <- train(Class ~ ., data = training_dataset,
                             method = "nnet", #Neural Network with parameters size and decay
                             tuneGrid = optParameters,
                             trControl=trainControl(method="none")
      )
      #save model 
      #  project_dir <- server_$getProjectDir()
      project_dir <- "/home/elena/R_ws/ADS/ADS_workspace/project_temp/model/model_files/"
      model_file <- paste(project_dir, "ann_model.rds", sep = "")
      saveRDS(trained_model, model_file)
      return(trained_model)
    },
    initialize=function(...) {
      size_ <<- 0
      decay_ <<- 0
      callSuper(...)
    }
)
