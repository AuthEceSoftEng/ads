##' Α class responsible for performing PCA and MDA analysis on a dataset.
##'
##' DataCompressor uses package caret to find PCA features which maintain the desired variance.
##' @import methods caret
##' @export
DataCompressor <- setRefClass(Class = "DataCompressor",
                           fields = list(
                             info_ = "list"
                         ),
                           methods = list(
                             performPCA = function(dataset, variance = 0.95,  ...) {
                               'Finds PCA features with desired variance. Takes into account only numeric features.'
                               library(caret)
                             
                               # remove categorical attributes
                               variables <- names(dataset[sapply(dataset,class) == "factor"])
                               num_dataset <-  as.data.frame(dataset[, !(names(dataset) %in% variables)])
                               # remove Class
                               num_dataset$Class <- NULL
                               
                               # apply PCA
                               pre.total_data <-predict.preProcess(num_dataset, 
                                                                   method=c("BoxCox"),newdata=num_dataset)
                               
                               PCA <- prcomp(pre.total_data,
                                             center = TRUE,
                                             scale = TRUE) 
                               attributes_for_desired_variance <- (which(cumsum((PCA$sdev)^2) / sum(PCA$sdev^2)>variance)[1])
                               transformed_dataset <- as.data.frame(PCA$x[, 1:attributes_for_desired_variance])
                               # reappend Class
                               transformed_dataset$Class <- dataset$Class
                               # update data_compress_info_ with info about PCA
                               pca_info <- list(pertained_variance = variance, number_of_features = attributes_for_desired_variance)
                               info_$PCA <<- pca_info
                               return(as.data.frame(transformed_dataset))
                             },
                             performMDA = function(dataset,variance = 95, ...) {
                               library(FactoMineR)
                               variables <- names(dataset[sapply(dataset,class) == "factor"])
                               cat_dataset <-  dataset[, (names(dataset) %in% variables)]
                               transformed_dataset <- cat_dataset
                               # str(cat_dataset)
                               # cat_dataset$Class <- NULL
                               # str(cat_dataset)
                               # mca <- FactoMineR::MCA(cat_dataset[1:100,], ncp = ncol(cat_dataset), graph=FALSE)
                               # library(MASS)
                               # #mca <- MASS::mca(cat_dataset, nf = ncol(cat_dataset))
                               # #str(mca$rs)
                               # cat("finished mcas")
                               #  str(mca$eig)
                               #  str(mca$eig$`cumulative percentage of variance`)
                               # attributes_for_desired_variance <- min(which((mca$eig$`cumulative percentage of variance`>variance)==TRUE))
                               # cat(attributes_for_desired_variance )
                               # str(data.frame(mca$ind$coord))
                               # transformed_dataset <- data.frame(mca$ind$coord)[, 1:attributes_for_desired_variance]
                               # transformed_dataset$Class <- dataset$Class
                               return(as.data.frame(transformed_dataset))
                               
                             },
                             getInfo = function(...) {
                               'Return information about data compression'
                               return(info_)
                             },
                             initialize = function(...) {
                               info_ <<- list()
                               callSuper(...)
                               .self
                             }
                           )
)
