#' Α class responsible for optimizing hyperparameters using metalearning.
#'
#' Optimal hyperparameters are predicted by using already trained HypeParameterPrediction models in directory
#' [workspace/HPP/models](workspace/HPP/models). For each hyperparameter a grid of 
#' optimal values is predicted by determining prediction intervals using bootstrapping, 
#' 
#' @include FileManipulator.R 
#' 
#' @slot seed_ seed for control of randomness
#' @slot model_file_ name of file containing trained HPP model
#' @slot parameters_file_ name of file containting parameters of HPP model
#' @slot file_manipulator_ an instance of Class \code{\link{FileManipulator}}
#' @slot info_ list of information about Optimizer

#' @import methods
#' @import caret
#' @import plyr
#' 
#' @export
Optimizer <- setRefClass(Class = "Optimizer",
                          fields = list(
                            model_file_       = "character",
                            parameters_file_  = "character",
                            file_manipulator_ = "FileManipulator",
                            seed_             = "numeric",
                            info_             = "list"
                            )
                         )
Optimizer$methods(
  #' Optimize hyperparameters
  #' 
  #' Optimizing hyperparameters of machine learning models by predicting them 
  #' using the HPP models in directory  [workspace/HPP/models](workspace/HPP/models).
  #' 
  #' @name optimizeHParam
  #' @alias optimizeHParam
  #' 
  #' @param algorithm name of algorithm
  #' @param parameters name of hyperparameters
  #' @param metafeatures metafeatures of dataset
  #' 
  #' @return list of optimal hyperparameters
  optimizeHParam = function( algorithm, parameters,  metafeatures,  ...) {
    'Returns a list of optimized hyperparameters'
    parameters_list <- list()
    for(i in 1:length(parameters)) {
      p                   <- parameters[i]
      # load neccessary information
      model               <- file_manipulator_$loadHppModel(name = paste(algorithm, p, model_file_, sep = "/"))
      model_info          <- file_manipulator_$loadHPPModelInfo(name = paste(algorithm, p, parameters_file_, sep = "/") )
      # preprocess metafeatures according to HPP model
      metafeatures_chosen <- as.data.frame(metafeatures[,names(metafeatures) %in% model_info$metafeatures ])
      metafeatures_chosen <- as.data.frame(scale(metafeatures_chosen, center = model_info$means, scale = model_info$scales))
      # predict optimal hyperparameter value
      optimal_p           <- predict(model, metafeatures_chosen)
      # determine prediction interval
      prediction          <- bootPi(model = model, pdata = metafeatures_chosen, n = model_info$n_boot[1],
                                     p = model_info$percentage[1], enableLog = model_info$enableLog[1])
      if(model_info$count) { # if hyperparameter is an integer
        prediction <- round(prediction)
      }
      interval             <- prediction[,c(2, 3)]
      # define grid
      parameter_vector     <- seq((unlist(interval[1])),unlist(interval[2]),model_info$step[1])
      parameters_list[[p]] <- parameter_vector
      # update information
      info_$size                                    <<- info_$size + length(parameter_vector)
      info_$results[1, p %in% names(info_$results)] <<- optimal_p
      info_$results[2, p %in% names(info_$results)] <<- interval[1]
      info_$results[3, p %in% names(info_$results)] <<- interval[2]
      info_$results[4, p %in% names(info_$results)] <<- model_info$step[1]
    }
    return(parameters_list)
  },
  #' Bootstrap prediction intervals
  #' 
  #' Determine prediction intervals of HypeParameterPrediction model using bootstrapping.
  #' Bootstrapping parameters like iterations and confidence level are determined by the HPP model.
  #' 
  #' @name bootPi
  #' @alias bootPi
  #' 
  #' @param model HPP model
  #' @param pdata data for prediction
  #' @param n number of iterations
  #' @param p confidence level
  #' @param enableLog apply log-transformation
  #' 
  #' @return list of optimal hyperparameters
  bootPi = function(model, pdata, n, p, enableLog = TRUE) { 
    registerDoParallel(cores = 4)
    params_list   <- list()
    optParameters <- model$bestTune
    odata <- model$trainingData
    # find bounds
    lp    <- (1 - p) / 2
    up    <- 1 - lp
    set.seed(seed_)
    seeds <- round(runif(n, 1, 1000), 0)
    boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
      set.seed(seeds[i])
      # define bootstrap sample
      bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
      if(enableLog) {
        model_current <- caret::train(log10(.outcome)~ ., data = bdata,
                                      method = model$method,
                                      tuneGrid = optParameters,
                                      trControl=trainControl(method="none"))
        bpred <- predict(model_current, newdata = pdata)
        bpred <- 10^bpred
      } else {
        model_current <- caret::train(.outcome~ ., data = bdata,
                                      method = model$method,
                                      tuneGrid = optParameters,
                                      trControl=trainControl(method="none"))
        bpred <- predict(model_current, newdata = pdata)
        bpred <- bpred
      }
      bpred
    }
    # find confidence interval
    boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up))) 
    # predict optimal value
    if(enableLog) { 
      predicted <- 10^predict(model, newdata = pdata)
    }
    else {
      predicted <- predict(model, newdata = pdata)
    }
    return(data.frame(pred = predicted, lower = boot_ci[, 1] , upper = boot_ci[, 2] ))
  },
  #' Return information about Optimizer
  #' 
  #' Information includes results of hyperparameter prediction
  #' 
  #' @name getInfo
  #' @alias getInfo
  #' 
  #' 
  #' @return list of information
  getInfo = function(...) {
    'Return information about hyperparameter optimization'
    return(info_)
  },
  initialize = function(...) {
    model_file_             <<- "model.RData"
    parameters_file_        <<- "parameters.csv"
    file_manipulator_       <<- FileManipulator$new()
    seed_                   <<- 1
    info_                   <<- list(size = 0, results = as.data.frame(matrix(nrow =4, ncol = 6)))
    colnames(info_$results) <<- c("k", "cp", "C", "sigma","size", "decay")
    callSuper(...)
    .self
  }
)
                            
                            
