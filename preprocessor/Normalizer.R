##' Α class responsible for normalizing the attributes of a dataset
##'
##' @import methods
##' @export
Normalizer <- setRefClass(Class = "Normalizer",
                            fields = list(
                              info_ = "list"
                            ),
                            methods = list(
                              zscoreNormalize = function(dataset, ...) {
                                'Normalizes dataset, so that numerical attributes have zero mean and one variance(categorical ones stay unchanged).'
                                variables <- names(dataset[sapply(dataset,class) == "factor"])
                                scaled_dataset <- dataset
                                scaled_dataset[, !(names(dataset) %in% variables)]  <- scale(dataset[, !(names(dataset) %in% variables)])
                                zscore_info <- list( applied = TRUE)
                                # zscore_info could contain mean and variance of each column(too bulky)
                                info_$zscore_info <<- zscore_info
                                return(scaled_dataset)
                              },
                              minMaxNormalize = function(dataset, range,  ...) {
                                'Normalizes dataset, so that numerical attributes have range 0-1(categorical ones stay unchanged).'
                                variables <- names(dataset[sapply(dataset,class) == "factor"])
                                num_names <- names(dataset)[!(names(dataset) %in% variables)]
                                scaled_dataset <- dataset
                                if(range == c("min","max")) {
                                  scaled_dataset[, !(names(dataset) %in% variables)]<- sapply(num_names, function(col) { (dataset[,col] - min(dataset[,col]))/(max(dataset[,col])-min(dataset[,col])) })
                                } else {
                                  desired_min <- range[1]
                                  desired_max <- range[2]
                                  scaled_dataset[, !(names(dataset) %in% variables)]<- sapply(num_names, function(col) { (dataset[,col] - desired_min)/(desired_max-desired_min) })
                                }
                                minmax_info <- list( applied = TRUE)
                                # zscore_info could contain min and max of each column(too bulky)
                                info_$minmax_info <<- minmax_info
                                return(scaled_dataset)
                              },
                              getInfo = function(...) {
                                'Return information about normalization'
                                return(info_)
                              },                              
                              initialize = function(...) {
                                info_ <<- list()
                                callSuper(...)
                                .self
                              }
                            )
)
