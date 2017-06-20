#' Α class responsible for extracting meta-meta-features of a dataset
#'
#' Level-2 metafeatures include: summation, mean, std, min, max, kurtosis and skewness.
#' 
#' @slot file_manipulator_ an instance of Class \code{\link{FileManipulator}}
#' @slot mf1_extractor_ an instance of Class \code{\link{mf1Extractor}}
#' 
#' @include mf1Extractor.R 
#' @include FileManipulator.R
#' 
#' @import methods
#' @import caret
#' @import plyr
#' 
#' @export
mf2Extractor <- setRefClass(Class = "mf2Extractor",
                            fields = list(
                              mf1_extractor_       = "mf1Extractor",
                              file_manipulator_    = "FileManipulator",
                              anticipation_metric_ = "list"
                              )
                            )
mf2Extractor$methods(
  #' Calculate meta-meta-features
  #' 
  #' Calculates statistical meta-features of metafeatures, which include
  #' summation, mean, standard deviation, minimum, maximum,
  #' kurtosis and skewness.
  #' 
  #' @name calculate2MetaFeatures
  #' @alias calculate2MetaFeatures
  #' 
  #' @param dataset input dataset
  #' 
  #' @return data.frame of meta-meta-features
  calculate2MetaFeatures = function(dataset, ...) {
    'Calculates level-2 metafeatures'
    names       <- c('sum.of.', 'mean.of.','std.of.',
                     'min.of.', 'max.of.', 'kurtosis.of.','skew.of.')
    variables   <- names(dataset[sapply(dataset,class) == "factor"])
    d_names     <- colnames(dataset)
    m2_features <- lapply(names, function(x) paste(x,d_names, sep = ""))
    m2_features <- unlist(m2_features)
    if(all(is.na(dataset)) ) {
      result           <- data.frame(as.list(rep(NA, length(m2_features)))) 
    }
    else {
      statFeats <- mf1_extractor_$calculateStatisticalNumeric(dataset)
      statFeats <- unlist(statFeats)
      result    <- as.data.frame(t(statFeats))
    }
    names(result) <- m2_features
    return(result)
  },
  #' Get meta-meta-features
  #' 
  #' Returns meta-meta-features of dataset by first calculating meta-features using
  #' \code{\link{mf1Extractor}}.
  #' 
  #' @name get2MetaFeatures
  #' @alias get2MetaFeatures
  #' 
  #' @param dataset input dataset
  #' 
  #' @return data.frame of meta-meta-features
  get2MetaFeatures = function(dataset, ...) {
    'Returns level-2 metafeatures of a dataset. This is the interface of the package'
    original_dataset <- dataset
    original_class   <- dataset$Class
    dataset$Class    <- NULL
    cat_features     <- names(dataset[sapply(dataset,class) == "factor" | (sapply(dataset,function(x) class(x)[1])  == "ordered")])
    dataset_cat      <- as.data.frame(original_dataset[ , (names(original_dataset) %in% cat_features)])
    dataset_num      <- as.data.frame(dataset[, !(names(dataset) %in% cat_features)])
    if((ncol(dataset_cat) == 0) && (ncol(dataset_num) == 0) ) {
      # empty dataset
      cat("Warning: provided empty dataset for meta-features extraction.")
      return(NULL)
    } else if((ncol(dataset_cat) != 0) && (ncol(dataset_num) != 0) ) {
      # dataset with both categorical and numeric features
      feats_num <- mf1_extractor_$calculateStatisticalNumeric(dataset = dataset_num)
      feats2_num <- calculate2MetaFeatures(feats_num)
      feats_cat <- mf1_extractor_$calculateStatisticalCategorical(dataset = dataset_cat)
      feats2_cat <- calculate2MetaFeatures(feats_cat)
      result    <- cbind(feats2_num, feats2_cat)
    } else if((ncol(dataset_cat) != 0)) {
      # dataset with only categorical features
      dataset_num <- mf1_extractor_$dummyEncode(dataset_cat)
      feats_num   <- mf1_extractor_$calculateStatisticalNumeric(dataset = dataset_num)
      feats2_num <- calculate2MetaFeatures(feats_num)
      feats_cat   <- mf1_extractor_$calculateStatisticalCategorical(dataset = dataset_cat)
      feats2_cat <- calculate2MetaFeatures(feats_cat)
      result      <- cbind(feats2_num, feats2_cat)
    } else if((ncol(dataset_num) != 0) ) {
      # dataset with only numerical features
      feats_num   <- mf1_extractor_$calculateStatisticalNumeric(dataset = dataset_num)
      feats2_num <- calculate2MetaFeatures(feats_num)
      feats_cat   <- mf1_extractor_$calculateStatisticalCategorical(dataset = dataset_num)
      feats2_cat <- calculate2MetaFeatures(feats_cat)
      result      <- cbind(feats2_num, feats2_cat)
    }
    # extract simple meta-features
    simpleFeats           <-  mf1_extractor_$calculateSimple(original_dataset)
    # extract information theoretic meta-features
    informFeats           <- mf1_extractor_$calculateInformationTheroretic(original_dataset)
    result                <- cbind(result, simpleFeats, informFeats)
    # replace NAs with zeros
    result[] <- lapply(result, function(x){
      x[is.nan(x)] <- 0
      x
    }  )
    # calculate anticipation metric
    anticipation_metric_ <<- calculateAnticipationMetric(dataset = result)
    return(result)
  },
  #' Calculate anticipation metric
  #' 
  #' Calculates anticipation metric of dataset, a value indicating how relevant it is
  #'  to our repository of datasets.
  #' 
  #' @name calculateAnticipationMetric
  #' @alias calculateAnticipationMetric
  #' 
  #' @param dataset input dataset
  #' 
  #' @return list with information about anticipation metric and if it indicates an outlier
  calculateAnticipationMetric = function(dataset, ... ) {
    distance_info <- list()
    # load metafeatures from repo
    repo_metafeatures <- file_manipulator_$loadRepoMetafeatures()
    # preprocess current dataset
    k       <- repo_metafeatures$info$optimal_k[1]
    means   <- repo_metafeatures$info$means
    scales  <- repo_metafeatures$info$scales
    dataset <- dataset[, names(dataset) %in% names(repo_metafeatures$info$means)]
    dataset <- scale(dataset, center = means, scale = scales)
    # calculate distance from all training examples
    distance <- apply(repo_metafeatures$dataset, 1, function(x) {sqrt(sum((x - dataset) ^ 2))})
    # find k-nearest examples
    distance <- distance[order(distance)[1:k]]
    # find average distance
    distance_info$value   <- mean(distance)
    distance_info$outlier <- (distance_info$value > repo_metafeatures$info$upper_bound[1]) 
    return(distance_info)
  },
  #' Get anticipation metric
  #' 
  #' @name getAnticipationMetric
  #' @alias getAnticipationMetric
  #' 
  #' @return list with information about anticipation metric and if it indicates an outlier
  getAnticipationMetric = function(...) {
    'Returns a metric characterizing how far current dataset lies from the datasets repo.'
    return(anticipation_metric_)
  },
  ##' @export
  initialize = function(...) {
    file_manipulator_    <<- FileManipulator$new()
    mf1_extractor_       <<- new('mf1Extractor')
    anticipation_metric_ <<- list()
    callSuper(...)
    .self
  }
)