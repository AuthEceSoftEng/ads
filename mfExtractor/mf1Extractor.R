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
                            min      <- apply( dataset_numeric, 2, function(x) myMin(x))
                            max      <- apply( dataset_numeric, 2, function(x) myMax(x))
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
                             # load landmarking accuracies
                             # calculate PCA metafeatures
                             # drop columns with zero-variance
                             dataset <- dataset[,colSums(is.na(dataset))<nrow(dataset)]
                             dataset[is.na(dataset)] <-0
                             uniquelength <- sapply(dataset,function(x) length(unique(x)))
                             dataset <- subset(dataset, select=uniquelength>1)
                             # drop Class for PCA
                             dataset_for_dim_red <- subset(dataset, select= -c(Class))
                             variables <- names(dataset_for_dim_red[sapply(dataset_for_dim_red,class) == "factor"])
                             if(length(variables)==0) {
                               dataset_for_PCA <- dataset_for_dim_red
                             }else {
                               dataset_for_PCA <- as.data.frame(dataset_for_dim_red[, - which(names(dataset_for_dim_red) %in% variables)])
                             } 
                             dataset_for_MDA <- as.data.frame(dataset_for_dim_red[, (names(dataset_for_dim_red) %in% variables)])
                             if(ncol(dataset_for_PCA)!= 0){
                               pre.total_data <-predict.preProcess(dataset_for_PCA, 
                                                                   method=c("BoxCox"),newdata=dataset_for_PCA)
                               
                             
                               pca <- prcomp(pre.total_data,
                                             center = TRUE,
                                             scale = TRUE) 
                               firstPC <- pca$x[,1]
                               firstPCkurt <- e1071::kurtosis(firstPC)
                               firstPCskew <- e1071::skewness(firstPC)
                               fraction_for_95_variance <- (which(cumsum((pca$sdev)^2) / sum(pca$sdev^2)>0.95)[1])/ncol(pca$x)
                             }else{
                               mca <- FactoMineR::MCA(dataset_for_MDA, graph=FALSE)
                               firstPC <- data.frame(mca$ind$coord)[,1]
                               firstPCkurt <- e1071::kurtosis(firstPC)
                               firstPCskew <- e1071::skewness(firstPC)
                               fraction_for_95_variance <- min(which((mca$eig$`cumulative percentage of variance`>95)==TRUE))/ncol(dataset_for_MDA)
                             }
                             # I didnt do anything for MDA
                             
                             
                             # calclulate skewness metafeatures
                             if(nrow(dataset_for_PCA)!=0) {
                               skew <- apply( dataset_for_PCA, 2, e1071::skewness, na.rm = TRUE)
                               skew_min <- min(skew)
                               skew_max <- max(skew)
                               skew_mean <- mean(skew)
                               skew_std <- sd(skew)
                               # calculate kurtosis metafeatures
                               kurt <- apply( dataset_for_PCA, 2, e1071::kurtosis, na.rm = TRUE)
                               kurt_min <- min(kurt)
                               kurt_max <- max(kurt)
                               kurt_mean <- mean(kurt)
                               kurt_std <- sd(kurt)
                               values <- list(firstPCkurt, firstPCskew, fraction_for_95_variance, 
                                              skew_min, skew_max, skew_mean, skew_std,
                                              kurt_min, kurt_max, kurt_mean, kurt_std   )
                             }else{
                               skew_min <- 0
                               skew_max <- 0
                               skew_mean <- 0
                               skew_std  <- 0
                               kurt_min <- 0
                               kurt_max <- 0
                               kurt_mean <- 0
                               kurt_std <- 0
                             }
                            
                             features <- features[7:length(features)]
                             result <- as.data.frame( values, col.names = features)
                             
                           },
                           ##' @export
                           initialize = function(...) {


                             callSuper(...)
                             .self
                           }
                         )
)
