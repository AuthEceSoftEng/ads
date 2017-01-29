##' Α class responsible for performing PCA and MDA analysis on a dataset.
##'
##' DataCompressor uses package caret to find PCA features which maintain the desired variance.
##' @import methods caret
##' @export
DataCompressor <- setRefClass(Class = "DataCompressor",
                           fields = list(
                             info_ = "list",
                             num_pca_attributes_ = "numeric",
                             num_mda_attributes_ = "numeric"
                         ),
                           methods = list(
                             performPCA = function(dataset, variance = 0.95, number_of_attributes = NULL, ...) {
                               'Finds PCA features with desired variance. Takes into account only numeric features.'
                               library(caret)
                               # remove categorical attributes
                               variables         <- names(dataset[sapply(dataset,class) == "factor"])
                               num_dataset       <-  as.data.frame(dataset[, !(names(dataset) %in% variables)])
                               # remove Class
                               num_dataset$Class <- NULL
                               # apply PCA
                               pre.total_data      <- predict.preProcess(num_dataset, 
                                                                   method=c("BoxCox"),newdata=num_dataset)
                               
                               PCA                 <- prcomp(pre.total_data,
                                                         center = TRUE,
                                                     scale = TRUE)
                               if(is.null(number_of_attributes)) {
                                 keep                <- (which(cumsum((PCA$sdev)^2) / sum(PCA$sdev^2)>=variance)[1])
                                 num_pca_attributes_ <<- keep
                               } else {
                                 keep                <- number_of_attributes
                               }
                               trans_dataset       <- as.data.frame(PCA$x[, 1:keep])
                               # reappend Class
                               trans_dataset$Class <- dataset$Class
                               # update info_ with info about PCA
                               pca_info  <- list(pertained_variance = variance, number_of_features = keep)
                               info_$PCA <<- pca_info
                               return(as.data.frame(trans_dataset))
                             },
                             performMDA = function(dataset,variance = 0.95, number_of_attributes = NULL, ...) {
                               library(FactoMineR)
                               library(MASS)
                               # prepare for MDA
                               variables           <- names(dataset[sapply(dataset,class) == "factor"])
                               cat_dataset         <-  dataset[, (names(dataset) %in% variables)]
                               transformed_dataset <- cat_dataset
                               cat_dataset$Class   <- NULL
                               # apply MDA
                               mca                 <- FactoMineR::MCA(cat_dataset, ncp = ncol(cat_dataset), graph=FALSE)
                               if(is.null(number_of_attributes)) {
                                 st_dev              <- apply(mca$ind$coord, 2, sd)
                                 keep                <- (which(cumsum((st_dev)^2) / sum(st_dev^2)>=variance)[1])
                                 num_mda_attributes_ <<- keep
                               } else {
                                 keep                <- number_of_attributes
                               }
                               trans_dataset       <- data.frame(mca$ind$coord)[, 1:keep]
                               trans_dataset$Class <- dataset$Class
                               # update info_ with info about PCA
                               mda_info  <- list(pertained_variance = variance, number_of_features = keep)
                               info_$MDA <<- mda_info
                               return(as.data.frame(trans_dataset))
                               
                             },
                             getNumPCAAttributes  = function(...) {
                               return(num_pca_attributes_)
                             },
                             getNumMDAAttributes  = function(...) {
                               return(num_mda_attributes_)
                             },
                             getInfo = function(...) {
                               'Return information about data compression'
                               return(info_)
                             },
                             initialize = function(...) {
                               info_               <<- list()
                               num_pca_attributes_ <<- 0
                               num_mda_attributes_ <<- 0
                               callSuper(...)
                               .self
                             }
                           )
)
