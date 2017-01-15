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
  trainModel = function(training_dataset, parameters, project_dir) {
    'Train a classification model.'
    optParameters <- expand.grid( k = c(parameters$k)
                                  )
    
    #train model
    trained_model <- train(Class ~ ., data = training_dataset,
                           method = "knn", #knn with parameters k(number of neighbors)
                           tuneGrid = optParameters,
                           trControl=trainControl(method="none")
    )
    #save model 
    model_dir <- file.path(project_dir, "model/model_files")
    model_file <- file.path(model_dir, "knn_model.rds")
    print(model_file)
    saveRDS(trained_model, model_file)
    return(trained_model)
  },
  initialize=function(...) {

    callSuper(...)
  }
)
