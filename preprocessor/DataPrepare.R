#' A class responsible for cleaning a dataset.
#' 
#' Cleaning includes conversion to appropriate features types and disposal of rare-levels for categorical features.
#' Partitiong of dataset is also supported.
#' 
#' @slot factor_threshold_  threshold percentage for determining a level categorical level
#' @slot info_ list of information about DataPrepare
#' 
#' @import methods
#' @import caret
#' 
#' @export
DataPrepare <- setRefClass(Class = "DataPrepare",
                           fields = list(
                             factor_threshold_ = "numeric",
                             info_ = "list"
                             )
                           )
DataPrepare$methods(
  #' Convert attribute types
  #' 
  #' Recognizes correct attribute types of a dataset. Availably types include
  #' numeric, ordered categorical and unordered categorical. Rare levels of categorical 
  #' attributes are dropped. Categorical attributes with zero or one level are dropped.
  #'
  #' @name convertAttributeTypes 
  #' @alias convertAttributeTypes 
  #' 
  #' @param dataset input dataset
  #' @param dictionary file with information about ordered attributes
  #' 
  #' @return converted dataset
  convertAttributeTypes = function(dataset, dictionary,...) {
    'Converts each attribute of a dataset to its right form.'
    converted_dataset        <- dataset
    # update data_prepare_info_ with total number of features (except Class)
    info_$number_of_features  <<- ncol(dataset) - 1
    info_$number_of_instances <<- nrow(dataset) - 1
    # drop NAs from Class
    converted_dataset <- converted_dataset[!is.na(converted_dataset$Class), ]
    # convert strings to factors
    variables                                            <- names(dataset[sapply(dataset,class) == "character"])
    converted_dataset[, (names(dataset) %in% variables)] <- lapply(as.data.frame(dataset[, (names(dataset) %in% variables)]), as.factor)
    factor_dataset                                      <- as.data.frame(converted_dataset[, (names(dataset) %in% variables)])
    # deal with factors with too many levels by assigning rare levels them to a level of insignificance
    converted_dataset[, (names(dataset) %in% variables)] <- disposeRareLevels( dataset = factor_dataset)
    # convert numeric with too few levels to factors
    factor_dataset              <- lapply(dataset, as.factor)
    num_files_factor            <- lapply(factor_dataset, nlevels)
    indexes                     <- which(num_files_factor<5)
    converted_dataset[,indexes] <- lapply(as.data.frame(converted_dataset[,indexes]), as.factor)
    # update info with names of factor_attributes
    info_$factor_attributes <<- c(variables, names(converted_dataset[,indexes]))
    # convert to ordinal factors based on dictionary
    variables        <- variables[variables != "Class"]
    dataset_temp     <- as.data.frame(converted_dataset[, (names(dataset) %in% variables)])
    if(ncol(dataset_temp) != 0) {
      is_value_ordered <- sapply(dataset_temp, function(x) length(intersect(x,dictionary$Values)) != 0)
      is_name_ordered  <- as.vector(unlist(lapply(names(dataset_temp), function(x) x %in% dictionary$Attributes)))
      if(is.null(is_value_ordered) | is.null(is_name_ordered)) is_ordered <- FALSE
      else is_ordered       <- is_value_ordered | is_name_ordered                               
      dataset_temp[, is_ordered == TRUE] <- lapply(as.data.frame(dataset_temp[, is_ordered == TRUE]), function(x) ordered(x))
      converted_dataset[, (names(dataset) %in% variables)] <- dataset_temp
      # update data_prepare_info with names of ordered_attributes
      info_$ordered_attributes <<- c(names(dataset_temp[, is_ordered == TRUE]))
    }
    # drop factors with zero levels or one level
    counter <- 0
    for(i in 1:ncol(converted_dataset)) {
      if(is.factor(converted_dataset[,i+counter])){
        temp <- converted_dataset[,i+counter]
        # replace NAs with zeros
        if(NA %in% temp) {
          temp                          <- factor(temp, levels=c(levels(temp), 0))
          temp[is.na(temp)]             <- 0
          converted_dataset[,i+counter] <- temp
        }
        if(nlevels((converted_dataset[,i+counter])) <=1 ) {
          converted_dataset[,i + counter] <- NULL
          counter                         <- counter - 1
        }
      }
    }
    converted_dataset$Class  <- factor(converted_dataset$Class, levels = levels(converted_dataset$Class), labels = c("Negative","Positive"))
    return(converted_dataset)
  },
  #' Find count features
  #' 
  #' Find features that represent count variables (positive integers).
  #'
  #' @name findCountFeatures 
  #' @alias findCountFeatures
  #' 
  #' @param dataset input dataset
  #' 
  #' @return subset of dataset containing only count attributes
  findCountFeatures = function(dataset, ...) {
    'Finds features that represent counts(non-negative integers)'
    # find numeric attributes
    integer_attributes   <- names(dataset[sapply(dataset,class) == "integer"])
    dataset_temp         <- as.data.frame(dataset[, (names(dataset) %in% integer_attributes)])
    names(dataset_temp)  <- integer_attributes
    is_attribute_count   <- apply(dataset_temp, 2,function(x) (x >=0) && is.integer(x))
    count_dataset        <- as.data.frame(dataset[, names(is_attribute_count[is_attribute_count==TRUE])]) 
    names(count_dataset) <- names(is_attribute_count[is_attribute_count==TRUE])
    return(count_dataset)
  },
  #' Dispose rare levels
  #' 
  #' Disposes levels of categorical attributes corresponding to less than \code{"factor_threshold_"} percentage.
  #'
  #' @name disposeRareLevels 
  #' @alias disposeRareLevels
  #' 
  #' @param dataset input dataset
  #' 
  #' @return dataset with rare levels disposed
  disposeRareLevels = function(dataset,  ... ) {
    'Finds rare levels in categorical attributes and reassigns them to a new level'
    frequencies             <- apply(dataset, 2, function(x) as.list(table(x)/length(x)))
    rare_frequencies        <- as.list(lapply(frequencies, function(x) which(x < factor_threshold_)))
    names(rare_frequencies) <-  colnames(dataset)
    return_dataset          <- as.data.frame(matrix(nrow = nrow(dataset), ncol = 1))
    # update data_prepare_info with names of compressed_attributes
    info_$compressed_attributes <<- c(names(dataset[,names(rare_frequencies)]))
    if(length(rare_frequencies) != 0) {
      for(i in seq(1, length(rare_frequencies))) {
        dataset_column <- dataset[,i]
        dataset_column <-  mapvalues(dataset_column, from = names(rare_frequencies[[i]]),
                                     to = rep("rare", length(rare_frequencies[[i]])))
        return_dataset <- cbind(return_dataset, dataset_column)
      }
      return_dataset[,1]       <- NULL
      colnames(return_dataset) <- names(dataset)
      # update info
      info_$rare_levels <<- rare_frequencies
    }
   
    return(return_dataset)
  },
  #' Partition data
  #' 
  #' Partition data according to requested technique: holdout, k-fold or leave-one-out.
  #'
  #' @name partitionData 
  #' @alias partitionData
  #' 
  #' @param dataset input dataset
  #' 
  #' @return partitions of dataset 
  partitionData = function(dataset, technique = list(name = "kfold", ratio = 0.9), ..) {
    'Returns training and testing partitions of data according to technique'
    if(technique$name == "holdout"){
      if((technique$ratio * nrow(dataset)) < 1) { # if desired percentage leaves test partition empty
        technique$ratio <- 1/(nrow(dataset))
      }
      partitions <- caret::createDataPartition( dataset$Class, p = technique$ratio, list = FALSE)
    }
    else if(technique$name == "kfold"){
      partitions <- caret::createDataPartition(times = 1/(1-technique$ratio), dataset$Class, p=technique$ratio, list = FALSE)
    }
    else if(technique$name == "loocv"){
      partitions <- caret::createFolds(y = dataset$Class, k = nrow(dataset), list = FALSE)
    }
    # update data_prepare_info with partitioning technique
    info_$partitioning <<- list( technique = technique$name, ratio = technique$ratio)
    return(partitions)
  },
  #' Get factor threshold
  #' 
  #' @name getFactorThreshold
  #' @alias getFactorThreshold
  #' 
  #' 
  #' @return factor threshold
  getFactorThreshold = function(...) {
    'Return factor_threshold_'
    return(factor_threshold_)
  },
  #' Return information about DataPrepare
  #' 
  #' Information includes recognized attribute types, disposed rare levels and partitioning.
  #' 
  #' @name getInfo
  #' @alias getInfo
  #' 
  #' 
  #' @return list of information
  getInfo = function(...) {
    'Returns information about data preparation'
    return(info_)
  },
  initialize=function(...) {
    factor_threshold_ <<- 0
    info_ <<- list()
    callSuper(...)
    .self
  }
  
)                           