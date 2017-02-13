##' Α class responsible for normalizing the attributes of a dataset
##'
##' @import methods
##' @export
Normalizer <- setRefClass(Class = "Normalizer",
                            fields = list(
                              info_ = "list",
                              zscore_attributes_ = "list",
                              minmax_attributes_ = "list"
                            ),
                            methods = list(
                              zscoreNormalize = function(dataset, precomputed, ...) {
                                'Normalizes dataset, so that numerical attributes have zero mean and one variance(categorical ones stay unchanged).'
                                variables         <- names(dataset[sapply(dataset,class) == "factor" | (sapply(dataset,function(x) class(x)[1])  == "ordered")])
                                num_names         <- names(dataset)[!(names(dataset) %in% variables)]
                                scaled_dataset    <- dataset
                                if(length(num_names ) != 0) {
                                if(precomputed == c("std","mean")) {
                                  scaled_dataset[, num_names]  <- scale(dataset[, num_names])
                                  zscore_attr <- attributes(scale(dataset[, num_names]))
                                  zscore_attributes_ <<- list(std_of_columns = as.vector(zscore_attr$`scaled:scale`), mean_of_columns = as.vector(zscore_attr$`scaled:center`))
                                } else {
                                  desired_std <- precomputed$std_of_columns
                                  desired_mean <- precomputed$mean_of_columns
                                  dataset_numeric <- as.data.frame(dataset[,num_names])
                                  for(i in 1:ncol(dataset_numeric)) {
                                    dataset_numeric[,i] <- (dataset_numeric[i] - desired_mean[i])/desired_std[i]
                                  }
                                  scaled_dataset[, num_names] <- dataset_numeric
                                  }
                               zscore_info       <- list( applied = TRUE)
                               info_$zscore_info <<- zscore_info
                                }
                               # zscore_info could contain mean and variance of each column(too bulky)
                               
                                return(scaled_dataset)
                              },
                              minMaxNormalize = function(dataset, precomputed,  ...) {
                                'Normalizes dataset, so that numerical attributes have range 0-1(categorical ones stay unchanged).'
                                variables      <-  names(dataset[sapply(dataset,class) == "factor" | (sapply(dataset,function(x) class(x)[1])  == "ordered")])
                                num_names      <- names(dataset)[!(names(dataset) %in% variables)]
                                scaled_dataset <- dataset
                                if(precomputed == c("min","max")) {
                                  scaled_dataset[, !(names(dataset) %in% variables)] <- sapply(num_names,
                                                                                               function(col) { (dataset[,col] - min(dataset[,col]))/(max(dataset[,col])-min(dataset[,col])) })
                                  min_list <- as.list(sapply(num_names, function(col)  min(dataset[,col]) ))
                                  max_list <- as.list(sapply(num_names, function(col)  max(dataset[,col]) ))
                                  minmax_attributes_ <<- list(min_of_columns = min_list, max_of_columns = max_list)
                                } else {
                                  desired_min <- precomputed$min_of_columns
                                  desired_max <- precomputed$max_of_columns
                                  scaled_dataset[, !(names(dataset) %in% variables)] <- sapply(num_names,
                                                                                               function(col) { (dataset[,col] - desired_min[[col]])/(desired_max[[col]]-desired_min[[col]]) })
                                }
                                # zscore_info could contain min and max of each column(too bulky)
                                minmax_info      <- list( applied = TRUE)
                                info_$minmax_info <<- minmax_info
                                return(scaled_dataset)
                              },
                              getZscoreAttributes = function() {
                                'Returns a list of mean and std of each column computed by zscore-normalization '
                                return(zscore_attributes_)
                              },
                              getMinMaxAttributes = function() {
                                'Returns a list of mean and std of each column computed by zscore-normalization '
                                return(minmax_attributes_)
                              },
                              getInfo = function(...) {
                                'Return information about normalization'
                                return(info_)
                              },                              
                              initialize = function(...) {
                                info_              <<- list()
                                zscore_attributes_ <<- list()
                                minmax_attributes_ <<- list()
                                callSuper(...)
                                .self
                              }
                            )
)
