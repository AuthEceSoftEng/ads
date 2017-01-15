##' A class responsible for training a k-nearest neighbor classifier
##' 
##' @include GenericClassifier.R 
##' @import methods
##' @export
BayesClassifier <- setRefClass(Class = "BayesClassifier",
                             fields = list(
                               fL_ = "numeric",
                               usekernel_ = "logical",
                               adjust_ = "numeric"
                             ),
                             contains = "GenericClassifier"
)

BayesClassifier$methods(
  trainModel = function(training_dataset) {
    'Train a classification model.'
    optParameters <- expand.grid( fL = c(fL_),
                                  usekernel = c(usekernel_),
                                  adjust = c(adjust_)
                                  )
    
    #train model
    trained_model <- train(Class ~ ., data = training_dataset,
                           method = "nb", #knn with parameters k(number of neighbors)
                           tuneGrid = optParameters,
                           trControl=trainControl(method="none")
    )
    #save model 
    #  project_dir <- server_$getProjectDir()
    project_dir <- "/home/elena/R_ws/ADS/ADS_workspace/project_temp/model/model_files/"
    model_file <- paste(project_dir, "bayes_model.rds", sep = "")
    saveRDS(trained_model, model_file)
    return(trained_model)
  },
  initialize=function(...) {
    fL_ <<- 1
    usekernel_ <<- TRUE
    adjust_ <<- 1
    callSuper(...)
  }
)