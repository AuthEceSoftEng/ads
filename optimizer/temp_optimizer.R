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
me <- paste(names(parameters$hyperparameters)[[i]], "W_high", sep = "_")
                                  resid_median <- model_info[[median_name]]
                                  pivot_quarts <- c(model_info[[low_name]], model_info[[high_name]])
                                  # get prediction intervals  from pivot quartiles
                                  predict_quants <- lapply(prediction, function(prediction) {
                                    return(c(resid_median - pivot_quarts[2]*abs(prediction - resid_median), resid_median - pivot_quarts[1]*abs(prediction - resid_median)))
                                  }
                                  )
                                  lwr <- lapply(predict_quants, function(x) {
                                    if(x<0){
                                      return(0.00001)
                                    } else {
                                    return(x[1])
                                    }
                                  })
                                  
                                  upr <- lapply(predict_quants, function(x) {
                                    return(x[2])
                                  })
                                  for_matrix <- c(prediction, lwr, upr )
                                  predictions_with_confidence <- matrix(for_matrix, nrow = nrow(metafeatures), ncol = 3 , byrow = FALSE)
                                  dimnames(predictions_with_confidence) = list( paste("dataset_", seq(1,nrow(metafeatures)), sep = ""),
                                                                                c("fit", "lwr", "upr")) # column names 
                                  interval <- predictions_with_confidence[,c("lwr", "upr")]
                                  parameter_vector <- seq((unlist(interval[1])),unlist(interval[2]),steps_svm[i])
                                }
                                else if(algorithm_ == "KnnClassifier") {
                                  trans <- model_info$Trans
                                  # load appropriate model
                                  predictions_with_confidence <- parameters$hyperparameters$k
                                  predictions_with_confidence[predictions_with_confidence < 0] <- 0.0001
                                  (predictions_with_confidence)
                                  if(trans==0){
                                    predictions_with_confidence[,"fit"] <- round(log(predictions_with_confidence[,"fit"]))
                                    predictions_with_confidence[,"lwr"] <- round(log(predictions_with_confidence[,"lwr"]))
                                    predictions_with_confidence[,"upr"] <- round(log(predictions_with_confidence[,"upr"]))
                                  }else{
                                    predictions_with_confidence[,"fit"] <- round((predictions_with_confidence[,"fit"]*trans +1)^(1/trans))
                                    predictions_with_confidence[,"lwr"] <- round((predictions_with_confidence[,"lwr"]*trans +1)^(1/trans))
                                    predictions_with_confidence[,"upr"] <- round((predictions_with_confidence[,"upr"]*trans +1)^(1/trans))
                                    
                                  }                                                                   
                                                                                   
                                  # n                <- model_info$Training_examples
                                  # s                <- model_info$Sd_Residuals
                                  # a                <- predictions
                                  # error            <- qnorm(0.975)*s/sqrt(n)
                                  # left             <- predictions - error
                                  # right            <- predictions + error
                                  # lwr              <- left 
                                  # upr              <- right
                                  # intervals        <- matrix(c(lwr, upr), nrow = nrow(metafeatures), ncol = 2 , byrow = FALSE)
                                  interval <- predictions_with_confidence[,c("lwr", "upr")]
                                  parameter_vector <- seq(round(interval[1]),round(interval[2]),1)
                                  
                                }
                                else if(algorithm_ == "TreeClassifier") {
                                  prediction <- parameters$hyperparameters$cp
                                  resid_median <- model_info$Laplace_median
                                  pivot_quarts <- c(model_info$W_low, model_info$W_high)
                                  # get prediction intervals  from pivot quartiles
                                  predict_quants <- lapply(prediction, function(prediction) {
                                    return(c(resid_median - pivot_quarts[2]*abs(prediction - resid_median), resid_median - pivot_quarts[1]*abs(prediction - resid_median)))
                                  }
                                  )
                                  lwr <- lapply(predict_quants, function(x) {
                                    return(x[1])
                                  })
                                  
                                  upr <- lapply(predict_quants, function(x) {
                                    return(x[2])
                                  })
                                  for_matrix <- c(prediction, lwr, upr )
                                  predictions_with_confidence <- matrix(for_matrix, nrow = nrow(metafeatures), ncol = 3 , byrow = FALSE)
                                  dimnames(predictions_with_confidence) = list( paste("dataset_", seq(1,nrow(metafeatures)), sep = ""),
                                                                                c("fit", "lwr", "upr")) # column names 
                                  interval <- predictions_with_confidence[,c("lwr", "upr")]
                                  parameter_vector <- seq((unlist(interval[1])),unlist(interval[2]),0.1)
                                }
                                else if(algorithm_ == "AnnClassifier") {
                                  if(i==1) {
                                    str(algorithm_)
                                    str(parameters$hyperparameters[[i]])
                                    prediction <- parameters$hyperparameters[[i]]
                                    median_name <- paste(names(parameters$hyperparameters)[[i]], "Laplace_median", sep = "_")
                                    low_name <- paste(names(parameters$hyperparameters)[[i]], "W_low", sep = "_")
                                    high_name <- paste(names(parameters$hyperparameters)[[i]], "W_high", sep = "_")
                                    resid_median <- model_info[[median_name]]
                                    pivot_quarts <- c(model_info[[low_name]], model_info[[high_name]])
                                    # get prediction intervals  from pivot quartiles
                                    predict_quants <- lapply(prediction, function(prediction) {
                                      return(c(resid_median - pivot_quarts[2]*abs(prediction - resid_median), resid_median - pivot_quarts[1]*abs(prediction - resid_median)))
                                    }
                                    )
                                    str(predict_quants)
                                    lwr <- lapply(predict_quants, function(x) {
                                      if(x<0){
                                        return(0.00001)
                                      } else {
                                        return(x[1])
                                      }
                                    })
                                    
                                    upr <- lapply(predict_quants, function(x) {
                                      return(x[2])
                                    })
                                    for_matrix <- c(prediction, lwr, upr )
                                    predictions_with_confidence <- matrix(for_matrix, nrow = nrow(metafeatures), ncol = 3 , byrow = FALSE)
                                    dimnames(predictions_with_confidence) = list( paste("dataset_", seq(1,nrow(metafeatures)), sep = ""),
                                                                                  c("fit", "lwr", "upr")) # column names 
                                    interval <- predictions_with_confidence[,c("lwr", "upr")]
                                    parameter_vector <- seq((unlist(interval[1])),unlist(interval[2]),steps_nnet[1])
                                  } else {
                                    trans <- model_info$Trans
                                    # load appropriate model
                                    predictions_with_confidence <- parameters$hyperparameters$size
                                    predictions_with_confidence[predictions_with_confidence < 0] <- 0.0001
                                    predictions_with_confidence[predictions_with_confidence > 9] <- 9
                                    str(predictions_with_confidence)
                                    if(trans==0){
                                      predictions_with_confidence[,"fit"] <- round(log(predictions_with_confidence[,"fit"]))
                                      predictions_with_confidence[,"lwr"] <- round(log(predictions_with_confidence[,"lwr"]))
                                      predictions_with_confidence[,"upr"] <- round(log(predictions_with_confidence[,"upr"]))
                                    }else{
                                      predictions_with_confidence[,"fit"] <- round((predictions_with_confidence[,"fit"]*trans +1)^(1/trans))
                                      predictions_with_confidence[,"lwr"] <- round((predictions_with_confidence[,"lwr"]*trans +1)^(1/trans))
                                      predictions_with_confidence[,"upr"] <- round((predictions_with_confidence[,"upr"]*trans +1)^(1/trans))
                                      
                                    }                                                                   
                                    predictions_with_confidence[,"upr"][predictions_with_confidence[,"upr"]  > 9] <- 9
                                    # n                <- model_info$Training_examples
                                    # s                <- model_info$Sd_Residuals
                                    # a                <- predictions
                                    # error            <- qnorm(0.975)*s/sqrt(n)
                                    # left             <- predictions - error
                                    # right            <- predictions + error
                                    # lwr              <- left 
                                    # upr              <- right
                                    # intervals        <- matrix(c(lwr, upr), nrow = nrow(metafeatures), ncol = 2 , byrow = FALSE)
                                    interval <- predictions_with_confidence[,c("lwr", "upr")]
                                    parameter_vector <- seq(round(interval[1]),round(interval[2]),steps_nnet[2])
                                    cat("entered")
                                    str(parameter_vector)
                                  }
                               
                                }
                                else if(algorithm_ == "BayesClassifier") {
                                }
                                intervals_list[[i]]  <- interval
                                str(parameter_vector)
                                parameters_list[[(names(parameters$hyperparameters)[[i]])]] <- parameter_vector
                                str(parameters_list)
                              }
                              return(parameters_list)
                            },
                            getModelPredictions = function(metafeatures, ...) {
                              str(algorithm_)
                              'Returns optimal hyperparameters predicted by model'
                              if(algorithm_ == "SvmClassifier") {
                                # predict optimal C
                                model_C       <- file_manipulator_$loadHppModel(name = paste(algorithm_, svm_model_C_, sep = "/"))
                                metafeatures[is.na(metafeatures)] <- 0
                                optimal_C     <-  kernlab::predict(model_C, metafeatures) 
                                str(optimal_C)
                                # predict optimal sigma
                                model_sigma       <- file_manipulator_$loadHppModel(name = paste(algorithm_, svm_model_sigma_, sep = "/"))
                                #str(model_sigma)
                                metafeatures[is.na(metafeatures)] <- 0
                                optimal_sigma     <-  kernlab::predict(model_sigma, metafeatures) 
                                str(optimal_sigma)
                                opt_parameters  <- list( hyperparameters= list(C = optimal_C, sigma = optimal_sigma),
                                                         models = list(CModel = model_C, sigmaModel = model_sigma))
                              }
                              else if(algorithm_ == "KnnClassifier") {
                                
                                # load appropriate model
                                model_k                           <- file_manipulator_$loadHppModel(name = paste(algorithm_, knn_model_k_, sep = "/"))
                                metafeatures[is.na(metafeatures)] <- 0
                                #str(model_k)
                                optimal_k      <- predict(model_k$finalModel, metafeatures, interval = "prediction", level = 0.5)
                                # make predictions
                                opt_parameters <- list( hyperparameters= list(k = optimal_k), model = model_k) 
                              }
                              else if(algorithm_ == "TreeClassifier") {
                                model_cp        <- file_manipulator_$loadHppModel(name = paste(algorithm_, tree_model_cp_, sep = "/"))
                                metafeatures[is.na(metafeatures)] <- 0
                                optimal_cp      <-  kernlab::predict(model_cp, metafeatures) 

                                str(optimal_cp)
                                # make predictions
                                opt_parameters  <- list( hyperparameters= list(cp = optimal_cp), model = model_cp)
                                str(opt_parameters)
                              }
                              else if(algorithm_ == "AnnClassifier") {
                                # predict decay
                                model_decay        <- file_manipulator_$loadHppModel(name = paste(algorithm_, nnet_model_decay_, sep = "/"))
                                metafeatures[is.na(metafeatures)] <- 0
                                optimal_decay      <-  kernlab::predict( model_decay , metafeatures)
                                
                                #predict size
                                model_size                          <- file_manipulator_$loadHppModel(name = paste(algorithm_, nnet_model_size_, sep = "/"))
                                metafeatures[is.na(metafeatures)] <- 0
                                #str(model_k)
                                optimal_size      <- predict(model_size$finalModel, metafeatures, interval = "prediction", level = 0.95)
                                opt_parameters  <- list( hyperparameters= list(decay = optimal_decay, size = optimal_size), models = list(model_decay, model_size))
                                str(opt_parameters)
                              }
                              else if(algorithm_ == "BayesClassifier") {
                              }
                              return(opt_parameters)
                              
                            },
                              initialize = function(...) {
                                knn_model_k_      <<- "lm_predicts_k.Rdata"
                                tree_model_cp_    <<- "ksvm_predicts_cp.Rdata"
                                svm_model_C_       <<- "ksvm_predicts_C.Rdata"
                                svm_model_sigma_   <<- "ksvm_predicts_sigma.Rdata"
                                nnet_model_decay_ <<- "ksvm_predicts_decay.Rdata"
                                nnet_model_size_ <<- "lm_predicts_size.Rdata"
                                file_manipulator_ <<- FileManipulator$new()
                                algorithm_        <<- ""
                                callSuper(...)
                                .self
                              }
                            )
)
