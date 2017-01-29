##' Α class responsible for removing inappropriate values from a dataset.
##'
##' As potential inappropriate values we consider unknown entries and outliers, which we treat accordingly
##' @import methods
##' @export
InapRemover <- setRefClass(Class = "InapRemover",
                          fields = list(
                            info_ = "list"

                          ),
                          methods = list(
                            removeUnknown = function(dataset, unknown_action = list( act = "delete", rep = 0), ...) {
                              'Treats unknown values according to the action (a list indicating whether na entries should lead to deletion of rows or replacement with desired value)'
                              dataset_treated <- dataset
                              na_info         <- list( number = sum(is.na(dataset)), technique = unknown_action$act)
                              info_$NAs       <<- na_info
                              if(unknown_action$act == "delete") {
                                dataset_treated <- na.omit(dataset)
                              }
                              else if(unknown_action$act == "replace") {
                                dataset_treated[is.na(dataset_treated)] <- unknown_action$rep
                              }
                              return(dataset_treated)
                            },
                            removeInfinites = function(dataset, inf_action = list( act= "delete", rep_pos = 1, rep_neg = 0),...) {
                              'Replace +Inf with and -Inf with desired values'
                              dataset_treated <- dataset
                              inf_info        <- list( number_positive = sum((dataset_treated =="Inf")),
                                                       number_negative = sum((dataset_treated =="-Inf")), technique = inf_action$act)
                              info_$Infs      <<- inf_info
                              if(inf_action$act == "delete") {
                                dataset_treated[dataset_treated =="Inf"]  <- NA
                                dataset_treated[dataset_treated =="-Inf"] <- NA
                              }
                              else if(inf_action$act == "replace") {
                                dataset_treated[dataset_treated =="Inf"]  <- rep_pos
                                dataset_treated[dataset_treated =="-Inf"] <- rep_neg
                              }
                              return(dataset_treated)
                            },
                            removeOutliers = function(dataset, thres_factor =1.5, outlier_action = list(rep = 0), ...) {
                              'Replaces outliers with desired value'
                              dataset_treated <- dataset
                              sum_below       <- 0
                              sum_above       <- 0
                              for(i in 1:length(dataset)) {
                                x                    <- dataset[,i]
                                qnt                  <- quantile(x, probs=c(.25, .75), na.rm = TRUE, ...)
                                H                    <- thres_factor * IQR(x, na.rm = TRUE) # iqr = upper_quantile - lower_quantile
                                y                    <- x
                                y[x< (qnt[1] - H)]   <- outlier_action$rep
                                y[x > (qnt[2] + H)]  <- outlier_action$rep
                                dataset_treated [,i] < -x
                                sum_below            <- sum_below + sum((x < (qnt[1] - H)))
                                sum_above            <- sum_above + sum((x > (qnt[1] - H)))
                                                                                          
                              }
                              out_info       <- list( number_below = sum_below, number_above = sum_above,
                                                replacement = outlier_action$rep)
                              info_$Outliers <<- out_info
                              return(dataset_treated)
                            },
                            getInfo = function(...) {
                              'Return information about removal of inappropriate values'
                              return(info_)
                            },
                            initialize = function(...) {
                                info_ <<- list()
                                callSuper(...)
                                .self
                              }
                            )
)
