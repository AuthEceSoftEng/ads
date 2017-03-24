##' Α class responsible for extracting metafeatures of a dataset
##'
##' Metafeatures include statistical (summation, mean, std, minimum, maximum, kurtosis, skewness)
##' \@references
##' \insertRef{mf1}{mfExtractor}
##' @import methods
##' @export mf1Extractor
##' @exportClass mf1Extractor
mf1Extractor <- setRefClass(Class = "mf1Extractor",
                            fields = list(
                              
                            ),
                            methods = list(
                              ##' @export
                                  myMax = function(x) {
                                    return(ifelse( !all(is.na(x)), max(x, na.rm=T), NA))
                                  } ,
                                  ##' @export
                                  myMin = function(x) {
                                    return(ifelse( !all(is.na(x)), min(x, na.rm=T), NA))
                                  } ,
                                  ##' @export
                                  calculateStatisticalNumeric = function(dataset, ...) {
                                    'Calculates statistical features of numeric attributes'
                                    features        <- c('sum', 'mean', 'std', 'min', 'max', 'kurtosis', 'skewness')
                                    dataset_numeric <- dataset
                                    if(is.null(dataset_numeric)) {
                                      return(as.data.frame(as.list(rep(NA, length(features))), col.names = features))
                                    }
                                    else {
                                      sum      <- apply( dataset_numeric, 2, sum, na.rm = TRUE)
                                      mean     <- apply( dataset_numeric, 2, mean, na.rm = TRUE)
                                      std      <- apply( dataset_numeric, 2, sd, na.rm = TRUE)
                                      #min      <- apply( dataset_numeric, 2, function(x) myMin(x))
                                      #max      <- apply( dataset_numeric, 2, function(x) myMax(x))
                                      min      <- apply( dataset_numeric, 2, min, na.rm = TRUE)
                                      max     <- apply( dataset_numeric, 2, max, na.rm = TRUE)
                                      kurtosis <- apply( dataset_numeric, 2, e1071::kurtosis, na.rm = TRUE)
                                      skewness <- apply( dataset_numeric, 2, e1071::skewness, na.rm = TRUE)
                                      values   <- list(sum, mean, std, min, max, kurtosis, skewness)
                                      result   <- as.data.frame( values, col.names = features)
                                      return(result)
                                    }
                                  },
                                  ##' @export
                                  calculateStatisticalCategorical = function(dataset, ...) {
                                    'Calculates statistical features of categorical attributes'
                                    dataset_categorical <- dataset
                                    if(is.null(dataset_categorical)) {
                                      return( data.frame( Levels = NA))
                                    }
                                    else {
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
                                  ##' @export
                                  calculateSimple = function(dataset, ...) {
                                    'Calculates simple(describing dataset on the whole, rather than per attribute) features'
                                    features           <- c('numFeatures', 'logNumFeatures', 'numPatterns',
                                                            'logNumPatterns', 'numPatternsNA', 'percPatternsNA', 
                                                            'numFeaturesNA', 'percFeaturesNA',  'numNA', 'percNA',
                                                            'logNumNAs','numNumericFeatures', 'numCatFeatures',
                                                            'classProbMin', 'classProbMax', 'classProbMean', 
                                                            'classProbStd')
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
                                    values <- list(numFeatures, logNumFeatures, numPatterns,
                                                   logNumPatterns,numPatternsNA,percnumPatternsNA,
                                                   numFeaturesNA, percnumFeaturesNA,numNAs,
                                                   percNA , logNumNAs, numNumericFeatures,
                                                   numCatFeatures , classProbMin, classProbMax,
                                                   classProbMean, classProbStd)
                                    result <- as.data.frame( values, col.names = features)
                                    return(result)
                                  },
                                  ##' @export
                                  calculateInformationTheroretic = function(dataset, ...) {
                                    'Calculates information theoretic features(Class entropy)'
                                    numPatterns <- nrow(dataset)
                                    p           <- table(dataset$Class)/numPatterns
                                    entropy     <- sum(-p*log(p))
                                    features    <- c("Class Entropy")
                                    values      <- list(entropy)
                                    result      <-as.data.frame( values, col.names = features)
                                    return(result)
                                    
                                  },
                                  ##' @export
                                  calculateLandmarking = function(dataset, ...) {
                                    'Calculates landmarking features(mention numerical or not)'
                                    # to be included in the future
                                  },
                                  calculateLandmarking = function(dataset, ...) {
                                    'Calculates landmarking features(mention numerical or not)'
                                    # to be included in the future
                                  },
                                  dummyEncode = function(dataset, ...) {
                                    # keep only numeric feature
                                    file_manipulator <- FileManipulator$new()
                                    test_dictionary  <- file_manipulator$loadOrderedDictionary()
                                    data_prepare     <- DataPrepare$new(factor_threshold_ = 1)
                                    str(dataset)
                                    dataset          <- data_prepare$convertAttributeTypes(dataset =  dataset , dictionary = test_dictionary)
                                    str(dataset)
                                    class <- dataset$Class
                                    variables        <-  names(dataset[sapply(dataset,class) == "factor" | (sapply(dataset,function(x) class(x)[1])  == "ordered")])
                                    dataset_num <- dataset[,!(names(dataset) %in% variables)]
                                    str(dataset_num)
                                    variables <- variables[variables != "Class"] 
                                    dataset_cat        <-  as.data.frame(dataset[, (names(dataset) %in% variables)])
                                    str(dataset_cat)
                                    if(length(variables) != 0) {
                                      data_prepare <- DataPrepare$new(factor_threshold_ = 0.1)
                                      dataset_cat <- data_prepare$disposeRareLevels(dataset = dataset_cat)
                                      counter <- 0
                                      converted_dataset <- dataset_cat
                                      remove <- c()
                                      for(i in 1:ncol(converted_dataset)) {
                                        if(is.factor(converted_dataset[,i+counter])){
                                          temp <- converted_dataset[,i+counter]
                                          # replace NAs with zeros
                                          if(NA %in% temp) {
                                            temp = factor(temp, levels=c(levels(temp), 0))
                                            temp[is.na(temp)] <- 0
                                            converted_dataset[,i+counter] <- temp
                                          }
                                          if(nlevels((converted_dataset[,i+counter])) <=1 ) {
                                            converted_dataset[,i + counter] <- NULL
                                            remove <- c(remove, i+ counter)
                                            counter <- counter -1
                                          }
                                        }
                                      }
                                      if(ncol(converted_dataset)!=0) {
                                        dmy <- caret::dummyVars(" ~ .", data = converted_dataset)
                                        trans <- data.frame(predict(dmy, newdata = dataset_cat ))
                                        if(!is.null(remove)) variables <- variables[-remove]
                                        dataset_cat <- trans
                                        dataset <- cbind(dataset_num, dataset_cat)
                                      } else {
                                        dataset <- dataset_num
                                      }
                                      
                                    } else {
                                      dataset <- dataset_num
                                    }
                                    dataset$Class <- class
                                    return(dataset)
                                  },
                                  
                                  ##' @import caret
                                  calculateAutosklearn = function(dataset,...) {
                                    'Calculates metafeatures proposed by autosklearn'
                                    library(caret)
                                    library(FactoMineR)
                                    features <- c("LandmarkLDA",
                                                  "LandmarkNaiveBayes",
                                                  "LandmarkDecisionTree",
                                                  "LandmarkRandomNodeLearner",
                                                  "Landmark1NN",
                                                  "Landmarknnet",
                                                  "PCAFractionOfComponentsFor95PercentVariance",
                                                  "PCAKurtosisFirstPC",
                                                  "PCASkewnessFirstPC",
                                                  "SkewnessMin",
                                                  "SkewnessMax",
                                                  "SkewnessMean",
                                                  "SkewnessSTD",
                                                  "KurtosisMin",
                                                  "KurtosisMax",
                                                  "KurtosisMean",
                                                  "KurtosisSTD")
                                    features <- features[7:length(features)]
                                    dataset <- dummyEncode(dataset)
                                    dataset_num <- dataset
                                    dataset_num$Class <- NULL
                                    # if(ncol( dataset_num )!=0) {
                                    inap_remover     <- InapRemover$new()
                                    dataset_num          <- inap_remover$removeInfinites(dataset =  dataset_num )
                                    dataset_num          <- inap_remover$removeUnknown(dataset =  dataset_num )
                                    
                                    data_compressor  <- DataCompressor$new()
                                    data_compressed  <- data_compressor$performPCA(dataset =  dataset_num , variance = 0.95)
                                    firstPC          <- data_compressed[,1]
                                    fractionFor95    <-  data_compressor$getNumPCAAttributes()/ncol( dataset_num )
                                    firstPCkurt      <- e1071::kurtosis(firstPC)
                                    firstPCskew      <- e1071::skewness(firstPC)
                                    
                                    # calclulate skewness metafeatures
                                    skew <- apply(  dataset_num , 2, e1071::skewness, na.rm = TRUE)
                                    str(skew)
                                    skew_min <- min(skew)
                                    skew_max <- max(skew)
                                    skew_mean <- mean(skew)
                                    skew_std <- sd(skew)
                                    # calculate kurtosis metafeatures
                                    kurt <- apply( dataset_num , 2, e1071::kurtosis, na.rm = TRUE)
                                    kurt_min <- min(kurt)
                                    kurt_max <- max(kurt)
                                    kurt_mean <- mean(kurt)
                                    kurt_std <- sd(kurt)
                                    values <- list(fractionFor95, firstPCkurt, firstPCskew,  
                                                   skew_min, skew_max, skew_mean, skew_std,
                                                   kurt_min, kurt_max, kurt_mean, kurt_std   )
                                    # }else{
                                    #   values <- list(fractionFor95 = NA, firstPCkurt= NA, firstPCskew= NA,  
                                    #                   skew_min= NA, skew_max= NA, skew_mean= NA, skew_std= NA,
                                    #                   kurt_min= NA, kurt_max= NA, kurt_mean= NA, kurt_std = NA  )
                                    # }
                                    result <- as.data.frame( values, col.names = features)
                                    
                                  },
                                  ##' @export
                                  initialize = function(...) {
                                    callSuper(...)
                                    .self
                                  }
                            )
)