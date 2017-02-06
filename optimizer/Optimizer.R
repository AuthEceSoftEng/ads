##' Α generic class defining the interface of an optimizer
##'
##' @import methods
##' @export
Optimizer <- setRefClass(Class = "Optimizer",
                          fields = list(
                            knn_model_k_      = "character",
                            file_manipulator_ = "FileManipulator",
                            algorithm_        = "character"

                          ),
                          methods = list(
                            optimizeHParamDefault = function(metafeature_dataset, algorithm,  ...) {
                              'Returns the list of optimal parameters for a specific machine learning algorithm and classification dataset.'
                              if(algorithm == "SvmClassifier") {
                                # load HPP model
                                # predict hyperparameters
                                # dummy values for testing
                                opt_parameters <- list(C = 1, sigma = 0.0001 )
                              }
                              else if(algorithm == "KnnClassifier") {
                                opt_parameters <- list(k = 1)
                              }
                              else if(algorithm == "TreeClassifier") {
                                opt_parameters <- list(maxdepth = 5, nu = 0.0001, iter =100 )
                              }
                              else if(algorithm == "AnnClassifier") {
                                opt_parameters <- list(size = 1, decay = 0.0001 )
                              }
                              else if(algorithm == "BayesClassifier") {
                                opt_parameters <- list(fL =  2, usekernel = FALSE, adjust = 0.2 )
                              }
                              return(opt_parameters)
                            },
                            optimizeHParam= function( algorithm, metafeatures,  ...) {
                              'Returns a list of optimized hyperparameters'
                                algorithm_                  <<- algorithm
                                # get one predicted value for each parameter of algorithm
                                optimal_predictions         <- getModelPredictions(metafeatures)
                                # get prediction intervals of model
                                optimal_predictions_list    <- getPredictionIntervals(parameters = optimal_predictions,metafeatures)
                                return(optimal_predictions_list)
                            },
                            getPredictionIntervals = function(parameters, metafeatures, ...) {
                              'Returns a matrix with lower and upper confidence bounds for each parameter of model'
                              intervals_list  <- list()
                              parameters_list <- list()
                              #str(parameters)
                              for (i in 1:length(parameters$hyperparameters)) {
                                model_info <- file_manipulator_$loadModelInfo(name = paste(algorithm_, "parameters.csv", sep = "/") )
                                if(algorithm_ == "SvmClassifier") {
                                  
                                }
                                else if(algorithm_ == "KnnClassifier") {
                                  # load appropriate model
                                  predictions      <- parameters$hyperparameters$k
                                  n                <- model_info$Training_examples
                                  s                <- model_info$Sd_Residuals
                                  a                <- predictions
                                  error            <- qnorm(0.975)*s/sqrt(n)
                                  left             <- predictions - error
                                  right            <- predictions + error
                                  lwr              <- left 
                                  upr              <- right
                                  intervals        <- matrix(c(lwr, upr), nrow = nrow(metafeatures), ncol = 2 , byrow = FALSE)
                                  parameter_vector <- seq(round(intervals[1]),round(intervals[2]),1)
                                  
                                }
                                else if(algorithm_ == "TreeClassifier") {
                                }
                                else if(algorithm_ == "AnnClassifier") {
                                }
                                else if(algorithm_ == "BayesClassifier") {
                                }
                                intervals_list[[i]]  <- intervals
                                parameters_list[[(names(parameters$hyperparameters)[[i]])]] <- parameter_vector
                              }
                              return(parameters_list)
                            },
                            getModelPredictions = function(metafeatures, ...) {
                              'Returns optimal hyperparameters predicted by model'
                              if(algorithm_ == "SvmClassifier") {
                                
                              }
                              else if(algorithm_ == "KnnClassifier") {
                                
                                # load appropriate model
                                model_k                           <- file_manipulator_$loadHppModel(name = paste(algorithm_, knn_model_k_, sep = "/"))
                                metafeatures[is.na(metafeatures)] <- 0
                                #str(model_k)
                                optimal_k      <- predict(model_k, metafeatures)
                                #str(optimal_k)
                                # make predictions
                                opt_parameters <- list( hyperparameters= list(k = optimal_k), model = model_k) 
                              }
                              else if(algorithm_ == "TreeClassifier") {
                              }
                              else if(algorithm_ == "AnnClassifier") {
                              }
                              else if(algorithm_ == "BayesClassifier") {
                              }
                              return(opt_parameters)
                              
                            },
                              initialize = function(...) {
                                knn_model_k_      <<- "svm_predicts_k.Rdata"
                                file_manipulator_ <<- FileManipulator$new()
                                algorithm_        <<- ""
                                callSuper(...)
                                .self
                              }
                            )
)
