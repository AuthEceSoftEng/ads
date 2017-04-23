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
  trainModel = function(training_dataset, parameters, file_manipulator) {
    'Train tree classification models. The names of trained models are returned.'
    # define parameters
    parameters    <- parameters[[1]]
    optParameters <- expand.grid( cp = c(parameters$cp))
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
      # models[[row]]      <- trained_model
      model_file <- file_manipulator$saveModel(model = trained_model, model_name = model_name_)
      model_files[[row]] <- model_file
    }
    return(model_files)
  },
  initialize=function(...) {
    model_name_ <<- "rpart"
    model_parameters_ <<- c("cp")
    callSuper(...)
  }
)
