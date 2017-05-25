##' Α generic class defining the interface of an optimizer
##'
##' @import methods
##' @export
Optimizer <- setRefClass(Class = "Optimizer",
                          fields = list(
                            model_file_       = "character",
                            parameters_file_  = "character",
                            file_manipulator_ = "FileManipulator",
                            algorithm_        = "character",
                            tree_model_cp_    = "character",
                            info_             = "list"
                          ),
                          methods = list(
                            optimizeHParamDefault = function(metafeature_dataset, algorithm,  ...) {
                              'Returns the list of optimal parameters for a specific machine learning algorithm and classification dataset.'
                              if(algorithm == "SvmClassifier") {
                                opt_parameters <- list(C = 1, sigma = 0.0001 )
                              }
                              else if(algorithm == "KnnClassifier") {
                                opt_parameters <- list(k = 1)
                              }
                              else if(algorithm == "TreeClassifier") {
                                opt_parameters <- list(cp=0.5 )
                              }
                              else if(algorithm == "AnnClassifier") {
                                opt_parameters <- list(size = 1, decay = 0.0001 )
                              }
                              else if(algorithm == "BayesClassifier") {
                                opt_parameters <- list(fL =  2, usekernel = FALSE, adjust = 0.2 )
                              }
                              return(opt_parameters)
                            },
                            optimizeHParam= function( algorithm, parameters,  metafeatures,  ...) {
                              'Returns a list of optimized hyperparameters'
                                algorithm_                  <<- algorithm
                                # get one predicted value for each parameter of algorithm
                                optimal_predictions_list         <- getModelPredictions(metafeatures, parameters)
                                return(optimal_predictions_list)
                            },
                            getModelPredictions = function(metafeatures, parameters, ...) {
                              'Returns optimal hyperparameters predicted by HPP model.'
                              parameters_list <- list()
                                for(i in 1:length(parameters)) {
                                  p                   <- parameters[i]
                                  model               <- file_manipulator_$loadHppModel(name = paste(algorithm_, p, model_file_, sep = "/"))
                                  model_info          <- file_manipulator_$loadModelInfo(name = paste(algorithm_, p, parameters_file_, sep = "/") )
                                  str(metafeatures)
                                  metafeatures_chosen <- as.data.frame(metafeatures[,names(metafeatures) %in% model_info$metafeatures ])
                                  str(metafeatures_chosen)
                                  str(model_info$means)
                                  metafeatures_chosen <- as.data.frame(scale(metafeatures_chosen, center = model_info$means, scale = model_info$scales))
                                  optimal_p           <- predict(model, metafeatures_chosen)
                                  prediction          <- boot_pi(model = model, pdata = metafeatures_chosen, n = model_info$n_boot[1],
                                                         p = model_info$percentage[1], enableLog = model_info$enableLog[1])
                                  if(model_info$count) {
                                    prediction <- round(prediction)
                                  }
                                  interval             <- prediction[,c(2, 3)]
                                  parameter_vector     <- seq((unlist(interval[1])),unlist(interval[2]),model_info$step[1])
                                  parameters_list[[p]] <- parameter_vector
                                  # update information
                                  info_$size           <<- info_$size + length(parameter_vector)
                                  info_$results[1, p %in% names(info_$results)] <<- optimal_p
                                  info_$results[2, p %in% names(info_$results)] <<- interval[1]
                                  info_$results[3, p %in% names(info_$results)] <<- interval[2]
                                  info_$results[4, p %in% names(info_$results)] <<- model_info$step[1]
                                }
                                return(parameters_list)
                            },
                            boot_pi = function(model, pdata, n, p, enableLog = TRUE) { 
                              registerDoParallel(cores = 4)
                              params_list <- list()
                              params <- names(model$bestTune)
                              x<- params[1]
                              optParameters <- data.frame( param= 2)
                              if(length(params)>1) {
                                for(i in 2:length(params)) {
                                  optParameters <- cbind(optParameters, data.frame( temp = model$bestTune[params[i]])) 
                                }
                              }
                              names(optParameters) <- params
                              odata <- model$trainingData
                              lp <- (1 - p) / 2
                              up <- 1 - lp
                              set.seed(1)
                              seeds <- round(runif(n, 1, 1000), 0)
                              boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
                                set.seed(seeds[i])
                                bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
                                if(enableLog) {
                                  model_current <- caret::train(log10(.outcome)~ ., data = bdata,
                                                        method = model$method,
                                                        tuneGrid = optParameters,
                                                        trControl=trainControl(method="none"))
                                  bpred <- predict(model_current, newdata = pdata)
                                  bpred <-10^bpred
                                } else {
                                  model_current <- caret::train(.outcome~ ., data = bdata,
                                                        method = model$method,
                                                        tuneGrid = optParameters,
                                                        trControl=trainControl(method="none"))
                                  bpred <- predict(model_current, newdata = pdata)
                                  bpred <-bpred
                                }
                                bpred
                              }
                              boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up))) 
                              if(enableLog) { 
                                predicted <- 10^predict(model, newdata = pdata)
                              }
                              else {
                                predicted <- predict(model, newdata = pdata)
                              }
                              return(data.frame(pred = predicted, lower = boot_ci[, 1] , upper = boot_ci[, 2] ))
                            },
                            getInfo = function(...) {
                              'Return information about hyperparameter optimization'
                              return(info_)
                            },
                              initialize = function(...) {
                                model_file_        <<- "model.RData"
                                parameters_file_   <<- "parameters.csv"
                                file_manipulator_  <<- FileManipulator$new()
                                algorithm_         <<- ""
                                info_              <<- list(size = 0, results = as.data.frame(matrix(nrow =4, ncol = 6)))
                                callSuper(...)
                                .self
                              }
                            )
)
