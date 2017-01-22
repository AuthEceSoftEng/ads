##' Α class responsible for assessing the performance of a trained model.
##'
##' Its use demands that the model has already been trained and the predictions on a test dataset have been extracted.
##' It offers various metrics of binary classification performance
##'
##'
##' @import methods
##' @exportClass PerformanceEvaluator
##' @export PerformanceEvaluator
PerformanceEvaluator <- setRefClass(Class = "PerformanceEvaluator",
                      fields = list(
                        predictions_ = "factor",
                        actual_class_ = "factor"
                        ),
                      methods = list(
                        calculateConfusionMatrixMetrics = function(selected_metrics, ...) {
                          'Returns a list of metrics (defined by the vector selected_metrics) derived from confusion matrix.'
                          # calculate confusion matrix
                          cm <- caret::confusionMatrix(predictions_, actual_class_)
                          TP <- cm$table[1, 1]
                          TN <- cm$table[2, 2]
                          FP <- cm$table[1, 2]
                          FN <- cm$table[2, 1]
                          # derive accuracy ((TP + TN) / (TP + TN + FP + FN))
                          accuracy <- as.numeric(cm$overall['Accuracy'])
                          # derive recall ((TP)/(TP + FN))
                          recall <- (TP)/(TP + FN)
                          # derive precision ((TP)/(TP + FP))
                          precision <- (TP)/(TP + FP)
                          # derive F-measure (2*(precision*recall)/(precision + recall))
                          Fmeasure <- 2*(precision*recall)/(precision + recall)
                          # derive Matthew's correlation coefficient ((TP * TN -FP * FN)/((TP + FP) * (TP + FN) *(TN + FP) * (TN + FN)))
                          Matthew_coeff <- (TP * TN -FP * FN)/((TP + FP) * (TP + FN) *(TN + FP) * (TN + FN))
                          # return list of metrics
                          conf_metrics <- list(Accuracy = accuracy, Recall = recall,
                                               Precision = precision, Fmeasure = Fmeasure, Matthew_coeff = Matthew_coeff)
                          return(conf_metrics[[selected_metrics]])
                        },
                        calculateAUC = function(predicted_probs, ...) {
                          'Returns area under ROC-curve' 
                          # probabilities extracted from trained model are needed
                          str(predicted_probs)
                          str(actual_class_)
                          roc_curve <- pROC::roc(predictor = predicted_probs$Positive,
                                         response = actual_class_
                                         )
                          return(roc_curve$auc)
                          #Area under the curve: 0.8731
                        },
                        calculateARI = function(...) {
                          'Returns adjusted Rank Index'
                          # if interested, look for implementation here
                          # http://link.springer.com/article/10.1007/BF01908075
                        },
                        setPredictions = function(predictions, ...) {
                          predictions_ <<- predictions
                        },
                        setActualClass = function(actual_class, ...) {
                          actual_class_ <<- actual_class
                        },
                        initialize = function(...) {
                           predictions_ <<- factor(0)
                           actual_class_ <<- factor(0)
                           callSuper(...)
                          .self
                        }
                      )
                      
)