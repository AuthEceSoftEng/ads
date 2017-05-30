#' Α class responsible for extracting metafeatures of a dataset
#'
#' Metafeatures include statistical, information-theoretic and simple.
#' 
#' @include GenericClassifier.R 
#' @include FileManipulator.R
#' @include DataPrepare.R
#' 
#' @import methods
#' @import caret
#' @import e1071
#' 
#' @export
mf1Extractor <- setRefClass(Class = "mf1Extractor",
                            fields = list())
                            
mf1Extractor$methods(
  #' Find maximum value
  #' 
  #' Find maximum value. If all is NA return NA, else ignore NA values.
  #' 
  #' @name myMax
  #' @alias myMax
  #' 
  #' @param x input vector
  #' 
  #' @return maximum value in vector
  myMax = function(x) {
    return(ifelse( !all(is.na(x)), max(x, na.rm=T), NA))
  } ,
  #' Find minimum value
  #' 
  #' Find minimum value. If all is NA return NA, else ignore NA values.
  #' 
  #' @name myMin
  #' @alias myMin
  #' 
  #' @param x input vector
  #' 
  #' @return minimum value in vector
  myMin = function(x) {
    return(ifelse( !all(is.na(x)), min(x, na.rm=T), NA))
  } ,
  #' Calculate statistical numeric metafeatures
  #' 
  #' Calculates statistical metafeatures of numeric attributes, which include summation, mean, standard
  #' deviation, minimum value, maximum value, kurtosis and skewness.
  #' 
  #' @name calculateStatisticalNumeric
  #' @alias calculateStatisticalNumeric
  #' 
  #' @param dataset_numeric dataset, containing only numeric attributes
  #' 
  #' @return data.frame of statistical numeric metafeatures
  calculateStatisticalNumeric = function(dataset_numeric, ...) {
    'Calculates statistical features of numeric attributes'
    features        <- c('sum', 'mean', 'std', 'min', 'max', 'kurtosis', 'skewness')
    if(is.null(dataset_numeric)) {
      return(as.data.frame(as.list(rep(NA, length(features))), col.names = features))
    } else {
      sum      <- apply( dataset_numeric, 2, sum, na.rm = TRUE)
      mean     <- apply( dataset_numeric, 2, mean, na.rm = TRUE)
      std      <- apply( dataset_numeric, 2, sd, na.rm = TRUE)
      min      <- apply( dataset_numeric, 2, function(x) myMin(x))
      max      <- apply( dataset_numeric, 2, function(x) myMax(x))
      kurtosis <- apply( dataset_numeric, 2, kurtosis, na.rm = TRUE)
      skewness <- apply( dataset_numeric, 2, skewness, na.rm = TRUE)
      values   <- list(sum, mean, std, min, max, kurtosis, skewness)
      result   <- as.data.frame( values, col.names = features)
      return(result)
    }
  },
  #' Calculate statistical categorical metafeatures
  #' 
  #' Calculates statistical metafeatures of categorical attributes, which include summation, mean, standard
  #' deviation, minimum value, maximum value, kurtosis and skewness.
  #' 
  #' @name calculateStatisticalCategorical
  #' @alias calculateStatisticalCategorical
  #' 
  #' @param dataset_categorical dataset, containing only categorical attributes
  #' 
  #' @return data.frame of statistical categorical metafeatures
  calculateStatisticalCategorical = function(dataset_categorical, ...) {
    'Calculates statistical features of categorical attributes'
    if(is.null(dataset_categorical)) {
      return( data.frame( Levels = NA))
    } else {
      feat       <- as.data.frame(matrix(NA, nrow = ncol(dataset_categorical), ncol = 10))
      features   <- c('Levels')
      num_levels <- as.data.frame(lapply(dataset_categorical, function(x) nlevels(x)))
      feat       <-as.data.frame(t(num_levels))
      if(nrow(feat)!=0) {
        names(feat)[1] <- "Levels"
      } 
      return(feat)
    }
  },
  #' Calculate simple metafeatures
  #' 
  #' Calculates simple metafeatures of dataset.
  #' 
  #' @name calculateSimple
  #' @alias calculateSimple
  #' 
  #' @param dataset input dataset
  #' 
  #' @return data.frame of simple metafeatures
  calculateSimple = function(dataset, ...) {
    'Calculates simple(describing dataset on the whole, rather than per attribute) features'
    features           <- c('numFeatures', 'logNumFeatures', 'numPatterns',
                            'logNumPatterns', 'numPatternsNA', 'percPatternsNA', 
                            'numFeaturesNA', 'percFeaturesNA',  'numNA', 'percNA',
                            'logNumNAs','numNumericFeatures', 'numCatFeatures',
                            'classProbMin', 'classProbMax', 'classProbMean', 
                            'classProbStd', 'PCAFractionOfComponentsFor95PercentVariance',
                            'PCAKurtosisFirstPC',
                            'PCASkewnessFirstPC')
    numFeatures        <- ncol(dataset)-1
    logNumFeatures     <- log(numFeatures)
    numPatterns        <- nrow(dataset)
    logNumPatterns     <- log(numPatterns)
    numPatternsNA      <- (rowMeans(is.na(dataset)) == 0)
    numPatternsNA      <- length(numPatternsNA[numPatternsNA==FALSE])
    percnumPatternsNA  <- numPatternsNA / numPatterns
    numFeaturesNA      <- (colMeans(is.na(dataset)) == 0)
    numFeaturesNA      <- length(numFeaturesNA[numFeaturesNA==FALSE])
    percnumFeaturesNA  <- numFeaturesNA / numFeatures
    numNAs             <- sum(is.na(dataset))
    percNA             <- numNAs/(numFeatures*numPatterns)
    logNumNAs          <- log(numNAs)
    numNumericFeatures <- length( names(dataset[sapply(dataset,class) == "integer"]))
    numCatFeatures     <- length( names(dataset[sapply(dataset,class) == "factor"]))
    classProbs         <- table(dataset$Class)/numPatterns
    classProbMin       <- min(classProbs)
    classProbMax       <- max(classProbs)
    classProbMean      <- mean(classProbs)
    classProbStd       <- sd(classProbs)
    dataset            <- dummyEncode(dataset)
    dataset_num        <- dataset
    dataset_num$Class  <- NULL
    inap_remover       <- InapRemover$new()
    dataset_num        <- inap_remover$removeInfinites(dataset =  dataset_num )
    dataset_num        <- inap_remover$removeUnknown(dataset =  dataset_num )
    
    data_compressor    <- DataCompressor$new()
    data_compressed    <- data_compressor$performPCA(dataset =  dataset_num , variance = 0.95)
    firstPC            <- data_compressed[,1]
    fractionFor95      <- data_compressor$getNumPCAAttributes()/ncol( dataset_num )
    firstPCkurt        <- kurtosis(firstPC)
    firstPCskew        <- skewness(firstPC)
    values             <- list(numFeatures, logNumFeatures, numPatterns,
                               logNumPatterns,numPatternsNA,percnumPatternsNA,
                               numFeaturesNA, percnumFeaturesNA,numNAs,
                               percNA , logNumNAs, numNumericFeatures,
                               numCatFeatures , classProbMin, classProbMax,
                               classProbMean, classProbStd, fractionFor95,
                               firstPCkurt, firstPCskew)
    result             <- as.data.frame( values, col.names = features)
    return(result)
  },
  #' Calculate information theoretic metafeatures
  #' 
  #' Calculates information theoretic metafeatures of dataset, which include Class entropy.
  #' 
  #' @name calculateInformationTheroretic
  #' @alias calculateInformationTheroretic
  #' 
  #' @param dataset input dataset
  #' 
  #' @return data.frame of information theoretic metafeatures
  calculateInformationTheroretic = function(dataset, ...) {
    'Calculates information theoretic features(Class entropy)'
    numPatterns <- nrow(dataset)
    p           <- table(dataset$Class)/numPatterns
    entropy     <- sum(-p*log(p))
    features    <- c("Class Entropy")
    values      <- list(entropy)
    result      <- as.data.frame( values, col.names = features)
    return(result)
    
  },
  #' Dummy encode dataset
  #' 
  #' Dummy encoding consists in converting each level of a categorical attribute to a binary attribute. 
  #' Its purpose is to consistently deal with categorical and numeric attributes, in our case extract
  #' the same set of metafeatures for all types of datasets. 
  #' 
  #' @name dummyEncode
  #' @alias dummyEncode
  #' 
  #' @param dataset input dataset
  #' 
  #' @return dummy encoded dataset
  dummyEncode = function(dataset, ...) {
    # recognize attribute types
    file_manipulator <- FileManipulator$new()
    #test_dictionary  <- file_manipulator$loadOrderedDictionary()
    #data_prepare     <- DataPrepare$new(factor_threshold_ = 1)
   # dataset          <- data_prepare$convertAttributeTypes(dataset =  dataset , dictionary = test_dictionary)
    class            <- dataset$Class
    variables        <- names(dataset[sapply(dataset,class) == "factor" | (sapply(dataset,function(x) class(x)[1])  == "ordered")])
    dataset_num      <- dataset[,!(names(dataset) %in% variables)]
    variables        <- variables[variables != "Class"] 
    dataset_cat      <- as.data.frame(dataset[, (names(dataset) %in% variables)])
    if(length(variables) != 0) {
      # dispore rare levels to avoid creation of too many binary attributes
      data_prepare      <- DataPrepare$new(factor_threshold_ = 0.1)
      dataset_cat       <- data_prepare$disposeRareLevels(dataset = dataset_cat)
      counter           <- 0
      converted_dataset <- dataset_cat
      remove            <- c()
      # replace NAs with zeros
      for(i in 1:ncol(converted_dataset)) {
        if(is.factor(converted_dataset[,i+counter])){
          temp <- converted_dataset[,i+counter]
          if(NA %in% temp) {
            temp                          <- factor(temp, levels=c(levels(temp), 0))
            temp[is.na(temp)]             <- 0
            converted_dataset[,i+counter] <- temp
          }
          if(nlevels((converted_dataset[,i+counter])) <= 1 ) {
            converted_dataset[,i + counter] <- NULL
            remove                          <- c(remove, i+ counter)
            counter                         <- counter -1
          }
        }
      }
      # dummy encode
      if(ncol(converted_dataset)!=0) {
        dmy   <- caret::dummyVars(" ~ .", data = converted_dataset)
        trans <- data.frame(predict(dmy, newdata = dataset_cat ))
        if(!is.null(remove)) {
          variables <- variables[-remove]
        } 
        dataset_cat <- trans
        dataset     <- cbind(dataset_num, dataset_cat)
      } else {
        dataset <- dataset_num
      }
    } else {
      dataset <- dataset_num
    }
    dataset$Class <- class
    return(dataset)
  },
  initialize = function(...) {
    callSuper(...)
    .self
  }
)                            
                           
