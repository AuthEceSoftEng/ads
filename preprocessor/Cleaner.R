
##' A class responsible cleaning a dataset after its loading.
##'
##' @include Server.R
##' @import methods
##' @exportClass Cleaner
##' @export Cleaner
Cleaner <- setRefClass(Class = "Cleaner",
                           fields = list(
                           ),
                           methods = list(
                             convertAttributeTypes= function(dataset, dictionary,...) {
                               'Converts each attribute of a dataset to its right form.'
                               converted_dataset <- dataset
                               # convert strings to factors
                               variables <- names(dataset[sapply(dataset,class) == "character"])
                               converted_dataset[, (names(dataset) %in% variables)] <- lapply(dataset[, (names(dataset) %in% variables)], as.factor)
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
                               converted_dataset$Class <- as.factor(converted_dataset$Class)
                               return(converted_dataset)
                               
                             },
                             initialize=function(...) {
                               callSuper(...)
                               .self
                             }
                           )
)
