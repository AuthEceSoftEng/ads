#' Α class responsible for taking decisions during the experiment, substituting a real-life expert.
#' 
#' Expert, based on heuristics, takes decisions during a machine learning experiment, e.g.
#'  preprocessing of a dataset, evaluation of a machine learning model.
#'  
#' @slot server_ an instance of Class \ref{\link{Server}}
#' 
#' @include Server.R
#' @include FeatureEngineer.R
#' @include InapRemover.R
#' @include Normalizer.R
#' @include DataCompressor.R
#' @include PerformanceEvaluator.R
#' @include HypothesisTester.R
#'  
#' 
#' @export
Expert <- setRefClass(Class = "Expert",
                      fields = list(
                        feature_engineer_        = "FeatureEngineer",
                        inap_remover_            = "InapRemover",
                        normalizer_              = "Normalizer",
                        data_compressor_         = "DataCompressor",
                        processed_task_          = "list",
                        preprocessing_procedure_ = "list",
                        performance_evaluator_   = "PerformanceEvaluator",
                        hypothesis_tester_       = "HypothesisTester",
                        conf_level_              = "numeric"
                        )
                      )
Expert$methods(
  #' Choose performance metric
  #' 
  #' Chooses performance metric appropriate to experiment according to its characteristics.
  #'
  #' @name choosePerformanceMetric 
  #' @alias choosePerformanceMetric 
  #' 
  #' @param model_name name of model
  #' @param dataset input dataset
  #' 
  #' @return name of performance metric
  choosePerformanceMetric = function(model_name, dataset, ...) {
    'Chooses appropriate performance metric according to heuristics'
    performance_metric <- "Accuracy"
    return(performance_metric)
  },
  #' Process task
  #' 
  #' Process user's input task in order to define Expert's task.
  #'
  #' @name processTask 
  #' @alias processTask
  #' 
  #' @param task list with information about experiment
  #' @return task
  processTask = function(task, ...) {
    'Checks fields of task to extract information about appropriate preprocessing techniques.'
    processed_task_ <<- task
  },
  #' Choose normalization method
  #' 
  #' Choose appropriate normalization method 
  #'
  #' @name chooseNormalizationMethod 
  #' @alias chooseNormalizationMethod
  #' 
  #' @param dataset input dataset
  #' 
  #' @return name normalization method
  chooseNormalizationMethod = function(dataset, ...) {
    'Chooses between zscore and minmax normalization based on processed_task'
    if(!is.na(processed_task_$normalize)) {
      return(processed_task_$normalize)
    }else {
      # check if minmax is appropriate, else 
      return("zscore")
    }
  },
  #' Is compression required
  #'
  #' @name isCompressRequired
  #' @alias isCompressRequired
  #' 
  #' @param dataset input dataset
  #' 
  #' @return TRUE if PCA and MDA will be applied
  isCompressRequired = function(dataset, ...) {
    'Decides if PCA and/or MDA compression is required'
    if(!is.na(processed_task_$compress)) {
      return(processed_task_$compress)
    } else {
      # check if PCA and MDA shoud be avoided 
      return(TRUE)
    }
  },
  #' Is a log-trasformation required
  #'
  #' Returns indexes of columns requiring a log-transformation
  #' 
  #' @name isLogTransformRequired
  #' @alias isLogTransformRequired
  #' 
  #' @param dataset input dataset
  #' 
  #' @return indexes of columns to apply log-transformation
  isLogTransformRequired = function(dataset, ...) {
    'Decides if PCA and/or MDA compression is required'
    # to be implemented
    # check if feature is skewed or have a too wide range
    indexes <- NULL
    return(indexes)
  },
  #' Choose preprocessing
  #'
  #' Determines preprocessing steps and executes them
  #' 
  #' @name choosePreprocessing
  #' @alias choosePreprocessing
  #' 
  #' @param dataset input dataset
  #' @param user's input task
  #' 
  #' @return dataset after preprocessing
  choosePreprocessing = function(dataset, task, final = FALSE, ...) {
    'Makes decisions about preprocessing and performs it.'
    # process task to extract info about preprocessing
    if(!final) processTask(task)
    else processed_task_ <<- task
    # remove inappropriate values(NAs and infinites)
    inf_action     <- list()
    unknown_action <- list()
    if(is.na(processed_task_$inf_action)) {
      inf_action$act <- "replace"
    } else {
      inf_action$act <- processed_task_$inf_action
    }
    if(is.na(processed_task_$inf_replace)) {
      inf_action$rep <-  0
    } else {
      inf_action$rep <- processed_task_$inf_replace
    }
    if(is.na(processed_task_$unknown_action)) {
      unknown_action$act <- "replace"
    } else {
      unknown_action$act <- processed_task_$unknown_action
    }
    if(is.na(processed_task_$unknown_replace)) {
      unknown_action$rep <-  0
    } else {
      unknown_action$rep<- processed_task_$unknown_replace
    }
    dataset        <- inap_remover_$removeInfinites(dataset, inf_action = inf_action)
    dataset        <- inap_remover_$removeUnknown(dataset, unknown_action =  unknown_action)
    processed_task_$preprocess$inapproriate <<- list(NA_action = unknown_action, Inf_action = inf_action)
    # choose between minmax and zscore Normalization
    method <- chooseNormalizationMethod()
    if(method == "zscore") {
      dataset                                     <- normalizer_$zscoreNormalize(dataset)
      processed_task_$preprocess$normalize$zscore <<- normalizer_$getZscoreAttributes()
    } 
    else if(method == "minmax") {
      dataset                                     <- normalizer_$minMaxNormalize(dataset)
      processed_task_$preprocess$normalize$minmax <<- normalizer_$getMinMaxAttributes()
    }
    # choose if data should be compressed
    if(isCompressRequired(dataset)) {
      dataset_numeric                         <- data_compressor_$performPCA(dataset)
      dataset_cat                             <- data_compressor_$performMDA(dataset)
      if(ncol(dataset_numeric) == 0) {
        dataset <- dataset_cat
      } else if (ncol(dataset_cat) == 0) {
        dataset <- dataset_numeric
      }
      else {
        dataset                              <- cbind(dataset_numeric, dataset_cat)
      }
      dataset                                 <-  dataset[,unique(colnames(dataset))]
      processed_task_$preprocess$compress$PCA <<- list( num_attributes = data_compressor_$getNumPCAAttributes())
      processed_task_$preprocess$compress$MDA <<- list( num_attributes = data_compressor_$getNumMDAAttributes())
    }
    # choose feature transformation
    indexes <- isLogTransformRequired(dataset)
    if(!is.null(indexes)) {
      dataset <- feature_engineer_$applyLogTransform(dataset, indexes)
    }
    return(dataset)
  },
  #' Apply preprocessing
  #'
  #' Applies preprocessing, as defined in \code{"processed_tak_$preprocess"}
  #' 
  #' @name applyPreprocessing
  #' @alias applyPreprocessing
  #' 
  #' @param dataset input dataset
  #' 
  #' @return dataset after preprocessing
  applyPreprocessing = function(dataset, ...) {
    'Applies preprocessing to dataset based on info in preprocessing_procedure_'
    # retrieve info from preprocessing_procedure_ and apply it
    applied_dataset <- dataset
    preprocess_task <- processed_task_$preprocess
    if(!is.null(preprocess_task$inapproriate)) {
      if(!is.na(preprocess_task$inapproriate$Inf_action)) {
        applied_dataset <- inap_remover_$removeInfinites(dataset, unknown_action = preprocess_task$inapproriate$Inf_action)
      }
      if(!is.null(preprocess_task$inapproriate$NA_action)) {
        applied_dataset <- inap_remover_$removeUnknown(dataset = applied_dataset,
                                                       unknown_action = preprocess_task$inapproriate$NA_action)
      }
    }
    if(!is.null(preprocess_task$normalize)) {
      
      if(!is.null(preprocess_task$normalize$minmax)) {
        applied_dataset                                       <- normalizer_$minMaxNormalize(dataset = applied_dataset,
                                                                                             precomputed = preprocess_task$normalize$minmax)
      }
      else if(!is.null(preprocess_task$normalize$zscore)) {
        applied_dataset                                       <- normalizer_$zscoreNormalize(dataset = applied_dataset,
                                                                                             precomputed = preprocess_task$normalize$zscore)
      }
    }
    if(!is.null(preprocess_task$compress)) {
      if(!is.null(preprocess_task$compress$PCA)) {
        applied_dataset_numeric  <- data_compressor_$performPCA(dataset = applied_dataset,
                                                                number_of_attributes = preprocess_task$compress$PCA$num_attributes)
      }
      if(!is.null(preprocess_task$compress$MDA)) {
        applied_dataset_cat  <- data_compressor_$performMDA(dataset = applied_dataset,
                                                            number_of_attributes = preprocess_task$compress$MDA$num_attributes)
      }
      applied_dataset                                <-  cbind(applied_dataset_numeric, applied_dataset_cat)
      applied_dataset                                <-  applied_dataset[,unique(colnames(applied_dataset))]
    }
    return(applied_dataset)
  },
  #' Get performance
  #' 
  #' @name getPerformance
  #' @alias getPerformance
  #' 
  #' @param predictions vector of predicted values
  #' @param actual_class vector of class
  #' @param predicted_probs vector of predicted probabilities
  #' @param performance_metric name of performance metric
  getPerformance = function(predictions, actual_class, predicted_probs, performance_metric, ...) {
    'Returns performance of model'
    # choose appropriate performance metric based on heuristics
    performance <- c()
    # --- call Performance Evaluator ---
    # get metrics extracted from confusion matrix
    if(!is.null(predictions)) performance_evaluator_$setPredictions(predictions = predictions)
    performance_evaluator_$setActualClass(actual_class = actual_class)
    if(performance_metric == "auc" ) {
      performance <- performance_evaluator_$calculateAUC(predicted_probs = predicted_probs)
    } else {
      performance <- performance_evaluator_$calculateConfusionMatrixMetrics(selected_metrics = performance_metric,
                                                                            actual_class = actual_class, predictions = predictions)
    }
    return(performance)
  },
  #' Get partition ratio
  #' 
  #' Defines partition ratio based on heuristics
  #' 
  #' @name getPartitionRatio
  #' @alias getPartitionRatio
  #' 
  #' @param testing_technique
  #' @param N
  getPartitionRatio = function(testing_technique, N) {
    'Returns ratio of data instances used for training by exploiting heuristcs'
    if(testing_technique$name == "holdout") {
      testing_technique$ratio <- 1-N/5 # use one fifth of data for testing
    } else if (testing_technique$name == "kfold") {
      testing_technique$ratio <- 0.9 # use 10-fold 
    }
    return(testing_technique$ratio)
  },
  #' Get HypothesisTester
  #' 
  #' @name getHypothesisTester
  #' @alias getHypothesisTester
  #' 
  #' @return an instance of \code{\link{HypothesisTester}}
  getHypothesisTester = function(...) {
    'Returns object normalizer_'
    return(hypothesis_tester_)
  },
  #' Get PerformanceEvaluator
  #' 
  #' @name getPerformanceEvaluator
  #' @alias getPerformanceEvaluator
  #' 
  #' @return an instance of \code{\link{PerformanceEvaluator}}
  getPerformanceEvaluator = function(...) {
    'Returns object normalizer_'
    return(performance_evaluator_)
  },
  #' Get DataCompressor
  #' 
  #' @name getDataCompressor
  #' @alias getDataCompressor
  #' 
  #' @return an instance of \code{\link{DataCompressor}}
  getDataCompressor = function(...) {
    'Returns object data_compressor_'
    return(data_compressor_)
  },
  #' Get Normalizer
  #' 
  #' @name getNormalizer
  #' @alias getNormalizer
  #' 
  #' @return an instance of \code{\link{Normalizer}}
  getNormalizer = function(...) {
    'Returns object normalizer_'
    return(normalizer_)
  },
  #' Get InapRemover
  #' 
  #' @name getInapRemover
  #' @alias getInapRemover
  #' 
  #' @return an instance of \code{\link{InapRemover}}
  getInapRemover = function(...) {
    'Returns object normalizer_'
    return(inap_remover_)
  },
  #' Get FeatureEngineer
  #' 
  #' @name getFeatureEngineer
  #' @alias getFeatureEngineer
  #' 
  #' @return an instance of \code{\link{FeatureEngineer}}
  getFeatureEngineer = function(...) {
    'Returns object normalizer_'
    return(feature_engineer_)
  },
  #' Get HypothesisTester
  #' 
  #' @name getHypothesisTester
  #' @alias getHypothesisTester
  #' 
  #' @return an instance of \code{\link{HypothesisTester}}
  getHypothesisTester = function(...) {
    'Returns object normalizer_'
    return(hypothesis_tester_)
  },
  #' Get processed task
  #' 
  #' @name getProcessedTask
  #' @alias getProcessedTask
  #' 
  #' @return processed task
  getProcessedTask = function(...) {
    return(processed_task_)
  }, 
  #' Set processed task
  #' 
  #' @name setProcessedTask
  #' @alias setProcessedTask
  #' 
  #' @param task list of information about task
  setProcessedTask = function(task, ...) {
    processed_task_ <<- task
  }, 
  #' Check if t-test is applicable
  #' 
  #' @name tTestApplicable
  #' @alias tTestApplicable
  #' 
  #' @return TRUE if t-test is applicable
  isTTestApplicable = function(...) {
    'Checks if t-test is applicable for comparing two machine learning models. Conditions are that datasets are enough and performance of each model
    follows a normal distribution'
    return(FALSE)
  },
  #' Check if anova is applicable
  #' 
  #' @name isAnovaApplicable
  #' @alias isAnovaApplicable
  #' 
  #' @return TRUE if anova is applicable
  isAnovaApplicable = function(...) {
    'Checks if ANOVA is applicable for comparing multiple machine learning models. Conditions are that performance of each model
                          follows a normal distribution and variance of performances is equal for each model.'
    return(FALSE)
  },
  #' Compare models
  #' 
  #' Determines appropriate method for comparing algorithms and then applies comparison.
  #' 
  #' @name compareModels
  #' @alias compareModels
  #' 
  #' @param results data.frame of results
  #' @param task user's task
  #' 
  #' @return result of test
  compareModels = function(results, task, ...) {
    'Returns comparisons on machine learning models based on hypothesis testing and according to task'
    # decide upon kind of test
    # auc is default metric
    if(ncol(results) == 2) {
      # if conditions are met apply Student's t-test
      if(isTTestApplicable) {
        test_result <- hypothesis_tester_$rankTest(results, methods = c("ttest"), conf.level = conf_level_ )
      } else {
        test_result <- hypothesis_tester_$rankTest(results, methods = c("wilcoxon"), conf.level = conf_level_)
      }
    } 
    else if(ncol(results) > 2){
      # if conditions are met apply ANOVA
      if(isAnovaApplicable()) {
        test_result <- hypothesis_tester_$rankTest(results, methods = c("ANOVA"), conf.level= conf_level_)
      } else {
        test_result <- hypothesis_tester_$rankTest(results, methods = c("friedman"), conf.level = conf_level_)
      }
      # post-hoc test
      results$X <- NULL
      Performances <- data.frame(
        Performance = as.vector(t(results)),
        Method = factor(rep(c("knnGrid", "nnetGrid", "treeGrid", "ensembleGrid", "knnTpe", "nnetTpe", "treeTpe","ensembleTpe","automl"), 9)),
        Dataset = factor(rep(1:18, rep(9, 18))))
      post_hoc <- posthoc.friedman.nemenyi.test(Performance ~ Method | Dataset ,Performances)
      indexes <- post_hoc$p.value < 0.05
      indexes <- which(indexes==TRUE, arr.ind = TRUE)
      r_index <- rownames(post_hoc$p.value)[indexes[,1]]
      c_index <- colnames(post_hoc$p.value)[indexes[,2]]
      post_hoc$r_index <- r_index
      post_hoc$c_index <- c_index
    } else {
      cat("Error: You need more than one models to compare.")
      test_result <- NULL
    }
    test_result <- test_result[[1]]
    test_result$post_hoc <- post_hoc
    if(test_result$p.value > conf_level_) test_result$Null_Hypothesis <- FALSE
    else test_result$Null_Hypothesis <- TRUE
    # test_result is a list containing all information extracted from test
    test_result$conf_level  <- conf_level_
    return(test_result)
  },
  initialize = function(...) {
    feature_engineer_        <<- FeatureEngineer$new()
    inap_remover_            <<- InapRemover$new()
    normalizer_              <<- Normalizer$new()
    data_compressor_         <<- DataCompressor$new()
    processed_task_          <<- list(model = NULL , application = NULL, certain_range = NULL, pca_percentage = NULL,
                                      preprocess = list(normalize = NULL, compress = NULL, log_transformations = NULL, inapproriate = NULL))
    performance_evaluator_   <<- PerformanceEvaluator$new()
    hypothesis_tester_       <<- HypothesisTester$new()
    conf_level_              <<- 0.95
    callSuper(...)
    .self
  }
)
