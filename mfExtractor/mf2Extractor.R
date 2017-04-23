##' Α class responsible for extracting level-2 metafeatures of a dataset.
##'
##' Level-2 metafeatures include: summation, mean, std, min, max, kurtosis and skewness.
##' @references
##' \insertRef{mf2}{mfExtractor}
##' @include mf1Extractor.R
##' @import methods
##' @export mf2Extractor
##' @exportClass mf2Extractor
mf2Extractor <- setRefClass(Class = "mf2Extractor",
                            fields = list(
                              mf1_extractor_ = "mf1Extractor"
                            ),
                            ##' @import e1071 plyr
                            methods = list(
                              ##' @export
                                  calculate2MetaFeatures = function(dataset, ...) {
                                    'Calculates level-2 metafeatures'
                                    names       <- c('sum.of.', 'mean.of.','std.of.',
                                                     'min.of.', 'max.of.', 'kurtosis.of.','skew.of.')
                                    variables   <- names(dataset[sapply(dataset,class) == "factor"])
                                    d_names     <- colnames(dataset)
                                    m2_features <- lapply(names, function(x) paste(x,d_names, sep = ""))
                                    m2_features <-unlist(m2_features)
                                    if(all(is.na(dataset)) ) {
                                      result           <- data.frame(as.list(rep(NA, length(m2_features)))) 
                                    }
                                    else {
                                      statFeatsNumeric <- mf1_extractor_$calculateStatisticalNumeric(dataset)
                                      View(statFeatsNumeric)
                                      statFeatsNumeric <- unlist(statFeatsNumeric)
                                      str(statFeatsNumeric)
                                      result           <-as.data.frame(t(statFeatsNumeric))
                                    }
                                    names(result) <- m2_features
                                    return(result)
                                  },
                                  ##' @export
                                  get2MetaFeatures = function(dataset, choice = "total",  ...) {
                                    'Returns level-2 metafeatures of a dataset. This is the interface of the package'
                                    original_dataset <- dataset
                                    dataset <- mf1_extractor_$dummyEncode(dataset = dataset)
                                    if(choice == "total") {
                                      drops                 <- names(original_dataset[sapply(original_dataset,class) != "numeric"])
                                      dataset_cat           <- as.data.frame(original_dataset[ , (names(original_dataset) %in% drops)])
                                      dataset_numeric       <- dataset[, !(names(dataset) %in% c("Class"))]
                                      result                <- list()
                                      hasNum                <- data.frame(hasNumeric = FALSE )
                                      hasCat                <- data.frame(hasCategorical = FALSE )
                                      if(ncol(dataset_numeric) != 0) hasNum$hasNumeric     <- TRUE
                                      if(ncol(dataset_cat) != 0)     hasCat$hasCategorical     <- TRUE
                                      statFeatsNumeric      <- mf1_extractor_$calculateStatisticalNumeric(dataset = dataset_numeric)
                                      statsFeatsNumeric2    <- calculate2MetaFeatures(dataset=statFeatsNumeric)
                                      result<- statsFeatsNumeric2
                                      statFeatsCat          <- mf1_extractor_$calculateStatisticalCategorical(dataset =dataset_cat)
                                      statFeatsCat2         <- calculate2MetaFeatures(dataset=statFeatsCat)
                                      if(length(result)!=0) {
                                        result              <- cbind(result, statFeatsCat2)
                                        
                                      } else {
                                        result              <- statFeatsCat2
                                      } 
                                      simpleFeats           <-  mf1_extractor_$calculateSimple(original_dataset)
                                      informFeats           <- mf1_extractor_$calculateInformationTheroretic(original_dataset)
                                      result                <- cbind(result, simpleFeats, informFeats, hasNum, hasCat )
                                      result_autosklearn    <- mf1_extractor_$calculateAutosklearn(dataset = dataset)
                                      result                <- cbind(result, result_autosklearn[,c(1,2,3)])
                                    }
                                    else if(choice == "autosklearn") {
                                      result <- mf1_extractor_$calculateAutosklearn(dataset = dataset)
                                    }
                                    return(result)
                                  },
                                  ##' @export
                                  initialize = function(...) {
                                    mf1_extractor_ <<- new('mf1Extractor')
                                    callSuper(...)
                                    .self
                                  }
                            )
)