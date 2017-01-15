##' A class responsible for training a k-nearest neighbor classifier
##' 
##' @include GenericClassifier.R 
##' @import methods
##' @export
KnnClassifier <- setRefClass(Class = "KnnClassifier",
                             fields = list(
                               k_ = "numeric"
                             ),
                             contains = "GenericClassifier"
)

KnnClassifier$methods(
  trainModel = function(training_dataset) {
    'Train a classification model.'
    optParameters <- expand.grid( k = c(k_)
                                  )
    
    #train model
    trained_model <- train(Class ~ ., data = training_dataset,
                           method = "knn", #knn with parameters k(number of neighbors)
                           tuneGrid = optParameters,
                           trControl=trainControl(method="none")
    )
    #save model 
    #  project_dir <- server_$getProjectDir()
    project_dir <- "/home/elena/R_ws/ADS/ADS_workspace/project_temp/model/model_files/"
    model_file <- paste(project_dir, "knn_model.rds", sep = "")
    saveRDS(trained_model, model_file)
    return(trained_model)
  },
  initialize=function(...) {
    k_ <<- 2
    callSuper(...)
  }
)