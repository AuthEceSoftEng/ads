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
                                names       <- c('sum of', 'mean of','std of', 'min of', 'max of', 'kurtosis of','skew of')
                                variables   <- names(dataset[sapply(dataset,class) == "factor"])
                                d_names     <- colnames(dataset)
                                m2_features <- lapply(d_names, function(d_names) paste(names,d_names))
                                m2_features <-unlist(m2_features)
                                if(all(is.na(dataset)) ) {
                                  result           <- data.frame(as.list(rep(NA, length(m2_features)))) 
                                }
                                else {
                                  statFeatsNumeric <- mf1_extractor_$calculateStatisticalNumeric(dataset)
                                  statFeatsNumeric <- unlist(statFeatsNumeric)
                                  result           <-as.data.frame(t(statFeatsNumeric))
                                }
                                names(result) <- m2_features
                                return(result)
                              },
                              ##' @export
                              get2MetaFeatures = function(dataset, ...) {
                                'Returns level-2 metafeatures of a dataset. This is the interface of the package'
                                drops                 <- names(dataset[sapply(dataset,class) == "factor"])
                                dataset_cat           <- as.data.frame(dataset[ , (names(dataset) %in% drops)])
                                dataset_numeric       <- as.data.frame(dataset[ , !(names(dataset) %in% drops)])
                                result                <- list()
                                hasNum                <- data.frame(hasNumeric = FALSE )
                                hasCat                <- data.frame(hasCategorical = FALSE )
                                hasNum$hasNumeric     <- TRUE
                                statFeatsNumeric      <- mf1_extractor_$calculateStatisticalNumeric(dataset = dataset_numeric)
                                statsFeatsNumeric2    <- calculate2MetaFeatures(dataset=statFeatsNumeric)
                                result<- statsFeatsNumeric2
                                hasCat$hasCategorical <- TRUE
                                statFeatsCat          <- mf1_extractor_$calculateStatisticalCategorical(dataset =dataset_cat)
                                statFeatsCat2         <- calculate2MetaFeatures(dataset=statFeatsCat)
                                if(length(result)!=0) {
                                  result              <- cbind(result, statFeatsCat2)
                                  
                                } else {
                                  result              <- statFeatsCat2
                                } 
                                simpleFeats           <-  mf1_extractor_$calculateSimple(dataset)
                                informFeats           <- mf1_extractor_$calculateInformationTheroretic(dataset)
                                result                <- cbind(result, simpleFeats, informFeats, hasNum, hasCat )
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
