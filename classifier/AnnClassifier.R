##' A class responsible for training an Artificial Neural Network.
##'
##' @include GenericClassifier.R 
##' @import methods
##' @export
AnnClassifier <- setRefClass(Class = "AnnClassifier",
                             contains = "GenericClassifier",
                             fields = list(
                             )
                             )

AnnClassifier$methods(
    trainModel = function(training_dataset, parameters, file_manipulator) {
      'Train ANN classification models. The names of trained models are returned.'
      # define parameters
      parameters    <- parameters[[1]]
      optParameters <- expand.grid( size = c(parameters$size) ,
                                    decay = c(parameters$decay)
                                    )
      model_files <- list()
      for (row in 1:nrow(optParameters)) {
        current_parameters <- as.data.frame(lapply(optParameters, function(x) x[row]))
        # train model
        trained_model      <- suppressWarnings(train(Class ~ ., data = training_dataset,
                                                     method = model_name_,
                                                     tuneGrid = current_parameters,
                                                     MaxNWts = 99999999 , # this parameter is used by nnet to prohibit large training times
                                                     trControl=trainControl(method="none", classProbs =  TRUE))
                                               )
        # check if examples are adequate for training a model of this family
        if(row == nrow(optParameters)/2) {
          info_ <<- AdequateExamples(model = trained_model)
        }
        colnames(trained_model$trainingData)[which(names(trained_model$trainingData) == ".outcome")] <- "Class"
        model_file <- file_manipulator$saveModel(model = trained_model, model_name = model_name_)
        # keep name of file
        model_files[[row]] <- model_file
      }
      return(model_files)
    },
    AdequateExamples = function(model, ...) {
      'Returns a list object indicating VC-dimension and adequateness of examples.'
      N        <- nrow(model$trainingData)
      d_vc     <- length(coef(model$finalModel)) + 1
      info     <- list(d_vc = d_vc, adequate = FALSE)
      str(N)
      str(d_vc)
      if(N>= (10*d_vc)) {
        info$adequate <- TRUE
      }
      return(info)
    },
    initialize=function(...) {
      model_name_       <<- "nnet"
      model_parameters_ <<- c("size", "decay")
      callSuper(...)
    }
)
