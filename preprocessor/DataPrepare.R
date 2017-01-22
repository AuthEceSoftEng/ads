
##' A class responsible cleaning a dataset after its loading.
##'
##' @include Server.R
##' @import methods
##' @exportClass Cleaner
##' @export Cleaner
DataPrepare <- setRefClass(Class = "DataPrepare",
                           fields = list(
                             factor_threshold_ = "numeric"
                           ),
                           methods = list(
                             convertAttributeTypes= function(dataset, dictionary,...) {
                               'Converts each attribute of a dataset to its right form.'
                               converted_dataset <- dataset
                               # convert strings to factors
                               variables <- names(dataset[sapply(dataset,class) == "character"])
                               converted_dataset[, (names(dataset) %in% variables)] <- lapply(dataset[, (names(dataset) %in% variables)], as.factor)
                               factor_dataset <- as.data.frame(converted_dataset[, (names(dataset) %in% variables)])
                               # deal with factors with too many levels by assigning rare levels them to a level of insignificance
                               converted_dataset[, (names(dataset) %in% variables)] <- disposeRareLevels( dataset = factor_dataset)
                               # convert numeric with too few levels to factors
                               factor_dataset <- lapply(dataset, as.factor)
                               num_files_factor <- lapply(factor_dataset, nlevels)
                               indexes <- which(num_files_factor<5)
                               converted_dataset[,indexes] <- lapply(converted_dataset[,indexes], as.factor)
                               # convert to ordinal factors based on dictionary
                               variables <- variables[variables != "Class"]
                               dataset_temp <- converted_dataset[, (names(dataset) %in% variables)]
                               is_name_ordered <- sapply(names(dataset_temp), function(x) x %in% dictionary$Attributes)
                               dataset_temp[, is_name_ordered == TRUE] <- lapply(dataset_temp[, is_name_ordered == TRUE], function(x) ordered(x))
                               converted_dataset[, (names(dataset) %in% variables)] <- dataset_temp
                               # convert Class to factor even if it's numeric
                               converted_dataset$Class <- factor(converted_dataset$Class, levels = c(0,1), labels = c("Negative","Positive"))
                               return(converted_dataset)
                             },
                             disposeRareLevels = function(dataset,  ... ) {
                               'Finds rare levels in categorical attributes and reassigns them to a new level'
                               library(plyr)
                               frequencies <- apply(dataset, 2, function(x) table(x)/length(x))
                               mean_frequencies <- lapply(frequencies, function(x) mean(x))
                               factor_threshold_ <- mean(unlist(mean_frequencies))
                               rare_frequencies <- lapply(frequencies, function(x) which(x < factor_threshold_))
                               return_dataset <- as.data.frame(matrix(nrow = nrow(dataset), ncol = 1))
                               for(i in seq(1, length(rare_frequencies))) {
                                 cat(i)
                                 #dataset_column <- apply(as.data.frame(dataset[,i]), 2 , function(x) x)
                                 
                                 dataset_column <- apply(as.data.frame(dataset[,i]), 2, function(x) mapvalues(x, from = names(rare_frequencies[[i]]), to = rep("rare", length(rare_frequencies[[i]]))))
                                 return_dataset <- cbind(return_dataset, dataset_column)
                               }
                               return_dataset[,1] <- NULL
                               return(return_dataset)
                               
                             },
                             partitionData = function(dataset, technique = list(name = "kfold", ratio = 0.9), ..) {
                               'Returns training and testing partitions of data according to technique'
                               
                               if(technique$name == "holdout"){
                                 partitions <- caret::createDataPartition(times = 1, dataset$Class, p=technique$ratio, list = FALSE)
                               }
                               else if(technique$name == "kfold"){
                                 partitions <- caret::createDataPartition(times = 1/(1-technique$ratio), dataset$Class, p=technique$ratio, list = FALSE)
                               }
                               else if(technique$name == "loocv"){
                                 # not sure if -1.5 works for all datasets
                                 partitions <- caret::createDataPartition(times = nrow(dataset), dataset$Class, p = (nrow(dataset)-1.5)/nrow(dataset), list = FALSE)
                                 cat((nrow(dataset)-1)/nrow(dataset))
                               }
                               return(partitions)
                             },
                             initialize=function(...) {
                               factor_threshold_ <<- 0.005
                               callSuper(...)
                               .self
                             }
                           )
)
