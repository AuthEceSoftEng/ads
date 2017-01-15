##' A class responsible for training a tree classifier
##' 
##' @include GenericClassifier.R 
##' @import methods
##' @export
TreeClassifier <- setRefClass(Class = "TreeClassifier",
                             fields = list(
                               cp_ = "numeric"
                             ),
                             contains = "GenericClassifier"
)

TreeClassifier$methods(
  trainModel = function(training_dataset) {
    'Train a classification model.'
    optParameters <- expand.grid( cp = c(cp_)
    )
    
    #train model
    trained_model <- train(Class ~ ., data = training_dataset,
                           method = "rpart", #knn with parameters k(number of neighbors)
                           tuneGrid = optParameters,
                           trControl=trainControl(method="none")
    )
    #save model 
    #  project_dir <- server_$getProjectDir()
    project_dir <- "/home/elena/R_ws/ADS/ADS_workspace/project_temp/model/model_files/"
    model_file <- paste(project_dir, "tree_model.rds", sep = "")
    saveRDS(trained_model, model_file)
    return(trained_model)
  },
  initialize=function(...) {
    cp_ <<- 0.01
    callSuper(...)
  }
)