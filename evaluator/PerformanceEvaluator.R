#' Α class responsible for assessing the performance of a trained model.
#'
#' Its use demands that the model has already been trained and the predictions on a test dataset have been extracted.
#' It offers various metrics of binary classification performance, such as accuracy, precision, recall,
#' F-measure, Matthew coeeficient and AUC.
#' 
#' @slot predictions_
#' @slot actual_class_
#' @slot info_ list of information about PerformanceEvaluator
#' 
#' @include GenericClassifier.R 
#' @include FileManipulator.R
#' 
#' @import methods
#' @import pROC
#' @import caret
#' 
#' @export
PerformanceEvaluator <- setRefClass(Class = "PerformanceEvaluator",
                      fields = list(
                        predictions_ = "factor",
                        actual_class_ = "factor",
                        info_ = "list"
                        )
                      )
PerformanceEvaluator$methods(
  #' Calculate metrics derived from Confusion Matrix
  #' 
  #' Metrics derived from Confusion Matrix include accuracy, precision, recall,
  #' F-measure and  Matthew coeeficient.
  #' 
  #' @name calculateConfusionMatrixMetrics
  #' @alias calculateConfusionMatrixMetrics
  #' 
  #' @param selected_metrics a subset of desired metrics
  #' @param predictions vector of model's predictions
  #' @param actual_class class
  #' 
  #' @return NULL
  calculateConfusionMatrixMetrics = function(selected_metrics, predictions, actual_class, ...) {
    'Returns a list of metrics (defined by the vector selected_metrics) derived from confusion matrix.'
    result = tryCatch({
      # calculate confusion matrix
      cm <- caret::confusionMatrix(predictions, actual_class)
      TP <- as.numeric(cm$table[1, 1])
      TN <- as.numeric(cm$table[2, 2])
      FP <- as.numeric(cm$table[1, 2])
      FN <- as.numeric(cm$table[2, 1])
      # derive accuracy ((TP + TN) / (TP + TN + FP + FN))
      accuracy      <- as.numeric(cm$overall['Accuracy'])
      # derive recall ((TP)/(TP + FN))
      recall        <- (TP)/(TP + FN)
      # derive precision ((TP)/(TP + FP))
      precision     <- (TP)/(TP + FP)
      # derive F-measure (2*(precision*recall)/(precision + recall))
      Fmeasure      <- 2*(precision*recall)/(precision + recall)
      # derive Matthew's correlation coefficient ((TP * TN -FP * FN)/((TP + FP) * (TP + FN) *(TN + FP) * (TN + FN)))
      Matthew_coeff <- (TP * TN -FP * FN)/sqrt((TP + FP) * (TP + FN) *(TN + FP) * (TN + FN))
      # return list of metrics
      conf_metrics  <- list(Accuracy = accuracy, Recall = recall,
                            Precision = precision, Fmeasure = Fmeasure,
                            Matthew_coeff = Matthew_coeff)
      for(i in 1:length(selected_metrics)) {
        info_[[selected_metrics[i]]] <<-  conf_metrics[[selected_metrics[i]]]
      }
      conf_metrics[[selected_metrics]]
    }, error = function(e) {
      0
    })
    return(result)
  },
  #' Calculate Area Under Curve
  #' 
  #' 
  #' @name calculateAUC
  #' @alias calculateAUC
  #' 
  #' @param predicted_probs vector of predicted probabiities
  #' 
  #' @return AUC value
  calculateAUC = function(predicted_probs, ...) {
    'Returns area under ROC-curve' 
    roc_curve <- roc(predictor = predicted_probs$Positive,
                           response = actual_class_
    )
    info_$auc <<-  as.numeric(roc_curve$auc) 
    return(as.numeric(roc_curve$auc))
  },
  #' Set predictions
  #' 
  #' @name setPredictions
  #' @alias setPredictions
  #' 
  #' 
  setPredictions = function(predictions, ...) {
    predictions_ <<- predictions
  },
  #' Set actual class
  #' 
  #' @name setActualClass
  #' @alias setActualClass
  #' 
  #' 
  #' @return list of information
  setActualClass = function(actual_class, ...) {
    actual_class_ <<- actual_class
  },
  #' Return information about PerformanceEvaluator
  #' 
  #' Information includes names and values of performance metrics.
  #' 
  #' @name getInfo
  #' @alias getInfo
  #' 
  #' 
  #' @return list of information
  getInfo = function(...) {
    'Return information about evaluation of performance'
    return(info_)
  },
  initialize = function(...) {
    predictions_  <<- factor(0)
    actual_class_ <<- factor(0)
    info_         <<- list()
    callSuper(...)
    .self
  }  
)                    