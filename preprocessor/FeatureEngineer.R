#' Α class responsible for transforming the features by means of feature engineering.
#' 
#' Support transformations are Box-Cox and log.
#' 
#' @slot info_ list of information about FeatureEngineer
#' 
#' @import methods
#' @import caret
#' @import MASS
#' 
#' @export
FeatureEngineer <- setRefClass(Class = "FeatureEngineer",
                           fields = list(
                             file_manipulator_ = "FileManipulator",
                             info_ = "list"
                             )
                           )
FeatureEngineer$methods(
  #' Find optimal Box-Cox transformation
  #' 
  #' Finds lambda of optimal Box-Cox transformation using maximum
  #' likelihood estimation 
  #'
  #' @name findOptimalBoxCoxTransform 
  #' @alias findOptimalBoxCoxTransform 
  #' 
  #' @param dataset input dataset
  #' 
  #' @return optimal lambda
  findOptimalBoxCoxTransform = function(dataset, ...) {
    'Returns optimal parameter lambda of boxcox transformation for a given formula'
    bc           <- boxcox(Class ~ ., data = train_dataset, plotit = FALSE)
    lambda       <- bc$x[which.max(bc$y)]
    boxcox_info  <- list(lambda = lambda)
    info_$BoxCox <<- boxcox_info
    return(lambda)
  },
  #' Apply log-transformation
  #' 
  #' Negative values and values between 0 and 1 are set to zero. 
  #'
  #' @name applyLogTransform 
  #' @alias applyLogTransform  
  #' 
  #' @param dataset input dataset
  #' @param indexes columns to apply log-transformation on. If NULL, apply to whole dataset
  #' 
  #' @return transformed dataset
  applyLogTransform = function(dataset, indexes = NULL, ...) {
    'Applies log-transformation to features with special treatment for negative values and values between 0 and 1'
    if(is.null(indexes)) {
      indexes <- seq(1, ncol(dataset))
    }
    dataset[,indexes] <- (ifelse(abs(dataset[,indexes]) <= 1, 0, sign(dataset[,indexes])*log10(abs(dataset[,indexes]))))
    log_info                      <- list(attributes = names(dataset[,indexes])) 
    info_$Log_transformation      <<- log_info
    return(dataset)
  },
  #' Return information about FeatureEngineer
  #' 
  #' Information refers to  optimal Box-Cox and log transformation.
  #' 
  #' @name getInfo
  #' @alias getInfo
  #' 
  #' 
  #' @return list of information
  getInfo = function(...) {
    'Return information about feature engineering'
    return(info_)
  },
  initialize = function(...) {
    info_ <<- list() 
    callSuper(...)
    .self
  }
)

