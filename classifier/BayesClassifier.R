##' A class responsible for training a bayesian classifier.
##' 
##' @include GenericClassifier.R 
##' @import methods
##' @export
BayesClassifier <- setRefClass(Class = "BayesClassifier",
                               fields = list(),
                               contains = "GenericClassifier"
                               )

BayesClassifier$methods(
  trainModel = function(training_dataset, parameters, file_manipulator) {
    'Train Bayes classification models. The names of trained models are returned.'
    # define parameters
    parameters    <- parameters[[1]]
    optParameters <- expand.grid( fL = c(parameters$fL),
                                  usekernel = c(parameters$usekernel),
                                  adjust = c(parameters$adjust)
                                  )
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
      # keep name of file
      model_files[[row]] <- model_file
    }
    return(model_files)
  },
  initialize=function(...) {
    model_name_ <<- "nb"
    model_parameters_ <<- c("fL", "usekernel", "adjust")
    callSuper(...)
  }
)


