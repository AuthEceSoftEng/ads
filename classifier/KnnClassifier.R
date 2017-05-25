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
      # check if examples are adequate for training a model of this family
      if(row == nrow(optParameters)/2) {
        info_ <<- AdequateExamples(model = trained_model)
      }
      colnames(trained_model$trainingData)[which(names(trained_model$trainingData) == ".outcome")] <- "Class"
      model_file <- file_manipulator$saveModel(model = trained_model, model_name = model_name_)
      model_files[[row]] <- model_file
    }
    return(model_files)
  },
  AdequateExamples = function(model, ...) {
    'Returns a list object indicating VC-dimension and adequateness of examples.'
    N        <- nrow(model$trainingData)
    k        <- model$bestTune$k
    if(k < 3) { # based on https://www.quora.com/How-does-the-parameter-k-affect-the-VC-dimension-of-the-k-nearest-neighbor-classifier
      d_vc <- Inf
    } else {
      d_vc <- 2
    }
    info     <- list(d_vc = d_vc, adequate = FALSE)
    str(N)
    str(d_vc)
    if(N >= (10*d_vc)) {
      info$adequate <- TRUE
    }
    return(info)
  },
  initialize=function(...) {
    model_name_ <<- "knn"
    model_parameters_ <<- c("k")
    callSuper(...)
  }
)
