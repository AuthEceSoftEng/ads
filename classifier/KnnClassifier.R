##' A class responsible for training a k-nearest neighbor classifier.
##' 
##' @include GenericClassifier.R 
##' @import methods
##' @export
KnnClassifier <- setRefClass(Class = "KnnClassifier",
                             fields = list(),
                             contains = "GenericClassifier"
                             )

KnnClassifier$methods(
  trainModel = function(training_dataset, parameters, file_manipulator) {
    'Train k-nearest neighbor classification models. The names of trained models are returned.'
    # define parameters
    parameters    <- parameters[[1]]
    optParameters <- expand.grid( k = c(parameters$k))
    model_files <- list()
    for (row in 1:nrow(optParameters)) {
      current_parameters <- as.data.frame(lapply(optParameters, function(x) x[row]))
      # train model
      trained_model      <- suppressWarnings(train(Class ~ ., data = training_dataset,
                                                   method = model_name_,
                                                   tuneGrid = current_parameters,
                                                   trControl=trainControl(method="none", classProbs =  TRUE))
      )
      colnames(trained_model$trainingData)[which(names(trained_model$trainingData) == ".outcome")] <- "Class"
      model_file <- file_manipulator$saveModel(model = trained_model, model_name = model_name_)
      model_files[[row]] <- model_file
    }
    return(model_files)
  },
  initialize=function(...) {
    model_name_ <<- "knn"
    model_parameters_ <<- c("k")
    callSuper(...)
  }
)
