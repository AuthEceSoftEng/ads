#' Α class responsible for performing PCA and MDA analysis on a dataset.
#' 
#' @slot num_pca_attributes_ number of pca attributes
#' @slot num_mda_attributes_ number of mda attributes
#' @slot info_ list of information about DataCompressor

#' @import methods
#' @import caret
#' @import FactoMineR
#' 
#' @export
DataCompressor <- setRefClass(Class = "DataCompressor",
                              fields = list(
                                info_ = "list",
                                num_pca_attributes_ = "numeric",
                                num_mda_attributes_ = "numeric"
                                )
                              )
DataCompressor$methods(
  #' Perform PCA analysis
  #' 
  #' This function supports two modes. By performing PCA analysis during training this function maintains the desired variance.
  #' By applying PCA analysis during testing  \code{"num_pca_attributes_"} are pertained. 
  #'
  #' @name performPCA
  #' @alias performPCA
  #' 
  #' @param dataset input dataset
  #' @param variance percentage of pertained variance
  #' @param number_of_attributes number of attributes pertained
  #' 
  #' @return dataset after application of PCA
  performPCA = function(dataset, variance = 0.95, number_of_attributes = NULL, ...) {
    'Finds PCA features with desired variance. Takes into account only numeric features.'
    # ignore categorical attributes
    variables         <-  names(dataset[sapply(dataset,class) == "factor" | (sapply(dataset,function(x) class(x)[1])  == "ordered")])
    num_dataset       <-  as.data.frame(dataset[, !(names(dataset) %in% variables)])
    # ignore Class
    num_dataset$Class <- NULL
    # apply PCA
    if(ncol(num_dataset) != 0) {
      pre.total_data      <- num_dataset
      PCA                 <- prcomp(pre.total_data)
      if(is.null(number_of_attributes)) { # first time performing PCA
        keep                <- (which(cumsum((PCA$sdev)^2) / sum(PCA$sdev^2)>=variance)[1])
        num_pca_attributes_ <<- keep
        trans_dataset       <- as.data.frame(PCA$x[, 1:keep])
        colnames(trans_dataset) <- paste("PC", seq(1, keep), sep = "")
      } else { # applying PCA on test
        keep                <- number_of_attributes
        if(ncol(PCA$x) < keep) { # if features are correlated PCA may give fewer PC components
          diff                                      <- keep - ncol(PCA$x)
          empty_data_frame                          <- as.data.frame(matrix(nrow = nrow(PCA$x), ncol = diff))
          empty_data_frame[is.na(empty_data_frame)] <- 0
          colnames(empty_data_frame)                <- paste("PC", seq((ncol(PCA$x)+1) , keep), sep = "")
          keep                                      <- ncol(PCA$x)
          trans_dataset                             <- as.data.frame(PCA$x[, 1:keep])
          colnames(trans_dataset)                   <- paste("PC", seq(1,ncol(PCA$x)), sep = "")
          trans_dataset                             <- cbind(trans_dataset, empty_data_frame) # append empty data.frame
        } else {
          trans_dataset           <- as.data.frame(PCA$x[, 1:keep])
          colnames(trans_dataset) <- paste("PC", seq(1,keep), sep = "")
        } 
      }
      # update info_ with info about PCA
      pca_info  <- list(pertained_variance = variance, number_of_features = keep)
      info_$PCA <<- pca_info
    }
    else {
      trans_dataset    <- dataset
    }
    trans_dataset$Class <- dataset$Class
    return((trans_dataset))
  },
  #' Perform MDA analysis
  #' 
  #' This function supports two modes. By performing MDA analysis during training this function maintains the desired variance.
  #' By applying MDA analysis during testing  \code{"num_pca_attributes_"} are pertained. 
  #'
  #' @name performMDA
  #' @alias performMDA
  #' 
  #' @param dataset input dataset
  #' @param variance percentage of pertained variance
  #' @param number_of_attributes number of attributes pertained
  #' 
  #' @return dataset after application of MDA
  performMDA = function(dataset, variance = 0.95, number_of_attributes = NULL,  ...) {
    # ignore categorical attributes
    variables         <-  names(dataset[sapply(dataset,class) == "factor" | (sapply(dataset,function(x) class(x)[1])  == "ordered")])
    cat_dataset       <-  as.data.frame(dataset[, (names(dataset) %in% variables)])
    # ignore Class
    # cat_dataset$Class <- NULL
    # # apply MDA
    # if(ncol(cat_dataset) != 0) {
    #   mca                 <- MCA(cat_dataset, ncp = ncol(cat_dataset), graph=FALSE)
    #   if(is.null(number_of_attributes)) { # first time performing MDA
    #     st_dev              <- apply(mca$ind$coord, 2, sd)
    #     keep                <- (which(cumsum((st_dev)^2) / sum(st_dev^2)>=variance)[1])
    #     num_mda_attributes_ <<- keep
    #     trans_dataset       <- data.frame(mca$ind$coord)[, 1:keep]
    #     colnames(trans_dataset) <- paste("MD", seq(1, keep), sep = "")
    #   } else { # applying MDA on test
    #     keep                <- number_of_attributes
    #     if(ncol(mca$ind$coord) < keep) { # if features are correlated MDA may give fewer MD components
    #       diff                                      <- keep - ncol(mca$ind$coord)
    #       empty_data_frame                          <- as.data.frame(matrix(nrow = nrow(mca$ind$coord), ncol = diff))
    #       empty_data_frame[is.na(empty_data_frame)] <- 0
    #       colnames(empty_data_frame)                <- paste("MD", seq((ncol(mca$ind$coord)+1) , keep), sep = "")
    #       keep                                      <- ncol(mca$ind$coord)
    #       trans_dataset                             <- as.data.frame(mca$ind$coord[, 1:keep])
    #       colnames(trans_dataset)                   <- paste("MD", seq(1,ncol(mca$ind$coord)), sep = "")
    #       trans_dataset                             <- cbind(trans_dataset, empty_data_frame) # append empty data.frame
    #     } else {
    #       trans_dataset           <- as.data.frame(mca$ind$coord[, 1:keep])
    #       colnames(trans_dataset) <- paste("MD", seq(1,keep), sep = "")
    #     } 
    #   }
    #   # update info_ with info about PCA
    #   mda_info  <- list(pertained_variance = variance, number_of_features = keep)
    #   info_$MDA <<- mda_info
    # }
    # else {
    #   trans_dataset    <- dataset
    # }
    # trans_dataset$Class <- dataset$Class
    return((cat_dataset))
  },
  #' Get number of PCA attributes
  #' 
  #' @name getNumPCAAttributes
  #' @alias getNumPCAAttributes
  #' 
  #' 
  #' @return number of PCA attributes
  getNumPCAAttributes  = function(...) {
    return(num_pca_attributes_)
  },
  #' Get number of MDA attributes
  #' 
  #' @name getNumMDAAttributes
  #' @alias getNumMDAAttributes
  #' 
  #' 
  #' @return number of MDA attributes
  getNumMDAAttributes  = function(...) {
    return(num_mda_attributes_)
  },
  #' Return information about DataCompressor
  #' 
  #' Information includes pertained variance and features after application of PCA and MDA>
  #' 
  #' @name getInfo
  #' @alias getInfo
  #' 
  #' 
  #' @return list of information
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