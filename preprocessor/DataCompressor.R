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
                                  variables         <-  names(dataset[sapply(dataset,class) == "factor" | (sapply(dataset,function(x) class(x)[1])  == "ordered")])
                                  num_dataset       <-  as.data.frame(dataset[, !(names(dataset) %in% variables)])
                                  # remove Class
                                  num_dataset$Class <- NULL
                                  # apply PCA
                                  if(ncol(num_dataset) != 0) {
                                    pre.total_data      <- predict.preProcess(num_dataset, 
                                                                              method=c(),newdata=num_dataset)
                                    PCA                 <- prcomp(pre.total_data)
                                    if(num_pca_attributes_ == 0) {
                                      keep                <- (which(cumsum((PCA$sdev)^2) / sum(PCA$sdev^2)>=variance)[1])
                                      num_pca_attributes_ <<- keep
                                      trans_dataset       <- as.data.frame(PCA$x[, 1:keep])
                                      colnames(trans_dataset) <- paste("PC", seq(1, keep), sep = "")
                                    } else {
                                      keep                <- num_pca_attributes_
                                      if(ncol(PCA$x) < keep) {
                                        
                                        diff <- keep- ncol(PCA$x)
                                        empty_data_frame <- as.data.frame(matrix(nrow = nrow(PCA$x), ncol = diff))
                                        empty_data_frame[is.na(empty_data_frame)] <- 0
                                        colnames(empty_data_frame) <- paste("PC", seq((ncol(PCA$x)+1) , keep), sep = "")
                                        keep <- ncol(PCA$x)
                                        trans_dataset       <- as.data.frame(PCA$x[, 1:keep])
                                        colnames(trans_dataset) <- paste("PC", seq(1,ncol(PCA$x)), sep = "")
                                        trans_dataset       <- cbind(trans_dataset, empty_data_frame)
                                      } else {
                                        trans_dataset       <- as.data.frame(PCA$x[, 1:keep])
                                        colnames(trans_dataset) <- paste("PC", seq(1,keep), sep = "")
                                      } 
                                    }
                                    trans_dataset$Class <- dataset$Class
                                    # update info_ with info about PCA
                                    pca_info  <- list(pertained_variance = variance, number_of_features = keep)
                                    info_$PCA <<- pca_info
                                  }
                                  else {
                                    trans_dataset    <- dataset
                                  }
                                  # reappend Class
                                  trans_dataset$Class <- dataset$Class
                                  
                                  
                                  return((trans_dataset))
                                },
                                performMDA = function(dataset,variance = 0.95, number_of_attributes = NULL, ...) {
                                  library(FactoMineR)
                                  library(MASS)
                                  # prepare for MDA
                                  variables           <-  names(dataset[sapply(dataset,class) == "factor" | (sapply(dataset,function(x) class(x)[1])  == "ordered")])
                                  cat_dataset         <-  dataset[, (names(dataset) %in% variables)]
                                  transformed_dataset <- cat_dataset
                                  # cat_dataset$Class   <- NULL
                                  # # apply MDA
                                  # mca                 <- FactoMineR::MCA(cat_dataset, ncp = ncol(cat_dataset), graph=FALSE)
                                  # if( num_mda_attributes_ == 0) {
                                  #   st_dev              <- apply(mca$ind$coord, 2, sd)
                                  #   keep                <- (which(cumsum((st_dev)^2) / sum(st_dev^2)>=variance)[1])
                                  #   cat("first time")
                                  #   num_mda_attributes_ <<- keep
                                  # } else {
                                  #   keep                <- num_mda_attributes_
                                  # }
                                  # trans_dataset       <- data.frame(mca$ind$coord)[, 1:keep]
                                  # trans_dataset$Class <- dataset$Class
                                  # # update info_ with info about PCA
                                  # mda_info  <- list(pertained_variance = variance, number_of_features = keep)
                                  # info_$MDA <<- mda_info
                                  return(as.data.frame(transformed_dataset))
                                  
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