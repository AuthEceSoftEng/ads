##' Α class responsible for taking decisions during the experiment, substituting a real-life expert .
##'
##' Expert communicates with the Knowledge DB, which, based on heuristics, answers queries regarding the
##' preprocessing of a dataset.
##'
##' @import methods
##' @exportClass Expert
##' @export Expert
Expert <- setRefClass(Class = "Expert",
                               fields = list(
                                  feature_engineer_ ="FeatureEngineer",
                                  inap_remover_ = "InapRemover",
                                  normalizer_= "Normalizer",
                                  data_compressor_= "DataCompressor",
                                  processed_task_ = "list",
                                  preprocessing_procedure_ = "list",
                                  performance_evaluator_ = "PerformanceEvaluator",
                                  hypothesis_tester_ = "HypothesisTester",
                                  conf_level_ = "numeric"
                               ),
                               methods = list(
                                 formQuery = function(request, ...) {
                                   'Based on the request( a list object, whith fields describing the question
                                   towards the database) a query (string) is formed.'
                                   # to be completed after design of database
                                 },
                                 askKnowledgeDB = function(request, ...) {
                                   'Communicates with Knowledge DB '
                                   # form desired query
                                   formQuery(request)
                                   # locate database
                                   # set up connection with database
                                   # get response
                                   return(response)
                                 },
                                 choosePerformanceMetric = function(model_name, dataset, ...) {
                                   'Chooses appropriate performance metric according to heuristics'
                                   performance_metric <- "auc"
                                   return(performance_metric)
                                 },
                                 processTask = function(task, ...) {
                                   'Checks fields of task to extract information about appropriate preprocessing techniques'
                                   processed_task_ <<- list()
                                   if(!is.null(task$algorithm)) {
                                     if(task$algorithm == "NN") {
                                       processed_task_$certain_range <<- 1
                                       range = c(0,1)
                                     } 
                                   }
                                   if(!is.null(task$application)) {
                                     if(task$application %in% c("image")) {
                                       processed_task_$certain_range <<- 1
                                     range = c(0,255)
                                     }
                                   }
                                   # find out if another metric is required
                                   if(TRUE) {
                                     processed_task_$metrics <<- c("auc")
                                   }
                                },
                                 chooseNormalizationMethod = function(...) {
                                   'Chooses between zscore and minmax normalization based on processed_task'
                                   if(is.null(processed_task_$certain_range)) {
                                     return("zscore")
                                   }else {
                                     return("minmax")
                                   }
                                 },
                                 IsCompressRequired = function(dataset, ...) {
                                   'Decides if PCA and/or MDA compression is required'
                                   # to be implemented
                                   return(TRUE)
                                 },
                                 IsLogTransformRequired = function(dataset, ...) {
                                   'Decides if PCA and/or MDA compression is required'
                                   # to be implemented
                                   # check if feature is skewed or have a too wide range
                                   indexes <- NULL
                                   return(indexes)
                                 },
                                 choosePreprocessing = function(dataset, task, final = FALSE, ...) {
                                   'Makes decisions about preprocessing and performs it.'
                                   # process task to extract info about preprocessing
                                   if(!final) processTask(task)
                                   else processed_task_ <<- task

                                   # remove inappropriate values(NAs and infinites)
                                   dataset <- inap_remover_$removeUnknown(dataset, unknown_action = list(act="delete", rep=0))
                                   dataset <- inap_remover_$removeInfinites(dataset, inf_action = list(act="delete"))
                                   # choose between minmax and zscore Normalization
                                   method <- chooseNormalizationMethod()
                                   if(method == "zscore") {
                                     dataset <- normalizer_$zscoreNormalize(dataset)
                                     
                                   } 
                                   else if(method == "minmax") {
                                     dataset <- normalizer_$minMaxNormalize(dataset, range = processed_task_$min_max_range)
                                   }
                                   # choose if data should be compressed
                                   if(IsCompressRequired(dataset)) {
                                     dataset_numeric <- data_compressor_$performPCA(dataset)
                                     dataset_cat <- data_compressor_$performMDA(dataset)
                                     dataset <- cbind(dataset_numeric, dataset_cat)
                                     dataset <-  dataset[,unique(colnames(dataset))]
                                   }
                                   # choose feature transformation
                                   indexes <- IsLogTransformRequired(dataset)
                                   if(!is.null(indexes)) {
                                     dataset <- feature_engineer_$applyLogTransform(dataset, indexes)
                                   }
                                   # store preprocessing procedure in list (for later application on test_dataset)
                                   preprocessing_procedure_ <<- list()
                                   return(dataset)
                                 },
                                 applyPreprocessing = function(dataset, task, ...) {
                                   'Applies preprocessing to dataset based on info in preprocessing_procedure_'
                                   # retrieve info from preprocessing_procedure_ and apply it
                                   # preprocessed_dataset <- dataset
                                   # WARNING: temporary for testing, task should not be an argument
                                   preprocessed_dataset <- choosePreprocessing(dataset, task)
                                   return(preprocessed_dataset)
                                 },
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
                                     performance <- performance_evaluator_$calculateConfusionMatrixMetrics(selected_metric = performance_metric)
                                   }
                                     return(performance)
                                 },
                                 getHypothesisTester = function(...) {
                                  'Returns object normalizer_'
                                  return(hypothesis_tester_)
                                 },
                                 getPerformanceEvaluator = function(...) {
                                  'Returns object normalizer_'
                                  return(performance_evaluator_)
                                 },
                                 getDataCompressor = function(...) {
                                   'Returns object data_compressor_'
                                   return(data_compressor_)
                                 },
                                 getNormalizer = function(...) {
                                   'Returns object normalizer_'
                                   return(normalizer_)
                                 },
                                 getInapRemover = function(...) {
                                  'Returns object normalizer_'
                                  return(inap_remover_)
                                 },
                                getFeatureEngineer = function(...) {
                                  'Returns object normalizer_'
                                  return(feature_engineer_)
                                },
                                getHypothesisTester = function(...) {
                                  'Returns object normalizer_'
                                  return(hypothesis_tester_)
                                },
                                getProcessedTask = function(...) {
                                  return(processed_task_)
                                }, 
                                 tTestApplicable = function(...) {
                                   'Checks if t-test is applicable for compating two machine learning models. Conditions are that datasets are enough and performance of each model
                                   follows a normal distribution'
                                   return(FALSE)
                                 },
                                 anovaApplicable = function(...) {
                                   'Checks if ANOVA is applicable for compating multiple machine learning models. Conditions are that performance of each model
                                   follows a normal distribution and variance of performances is equal for each model.'
                                   return(FALSE)
                                 },
                                 compareModels = function(results, task, ...) {
                                   'Returns comparisons on machine learning models based on hypothesis testing and accordin to task'
                                   # decide upon kind of test
                                   # auc is default metric
                                   if(is.null(task$compare$metric)) task$compare$metric <- "auc"
                                   # extract performance metric from results
                                   results <- lapply(results, function(x) as.data.frame(x[, task$compare$metric]))
                                   if(length(results) == 2) {
                                     # if conditions are met apply Student's t-test
                                     if(tTestApplicable()) {
                                       test_result <- hypothesis_tester_$rankTest(results, methods = c("ttest"), conf.level = conf_level_ )
                                     }
                                     else {
                                       test_result <- hypothesis_tester_$rankTest(results, methods = c("wilcoxon"), conf.level = conf_level_)
                                     }
                                   } 
                                   else if(length(results)>2){
                                     # if conditions are met apply ANOVA
                                     if(anovaApplicable()) {
                                       test_result <- hypothesis_tester_$rankTest(results, methods = c("ANOVA"), conf.level= conf_level_)
                                     }
                                     else {
                                       test_result <- hypothesis_tester_$rankTest(results, methods = c("friedman"), conf.level = conf_level_)
                                     }
                                   }
                                   else {
                                     cat("Error: You need more than one models to compare.")
                                     test_result <- NULL
                                   }
                                   str(test_result)
                                   str(test_result$test_result)
                                   if(test_result$test_result$p.value > conf_level_)
                                   # test_result is a list containing all information extracted from test
                                   return(test_result)
                                 },
                                 initialize = function(...) {
                                   feature_engineer_ <<- FeatureEngineer$new()
                                   inap_remover_ <<- InapRemover$new()
                                   normalizer_ <<- Normalizer$new()
                                   data_compressor_ <<- DataCompressor$new()
                                   processed_task_ <<- list(model = NULL , application = NULL, certain_range = NULL, pca_percentage = NULL)
                                   preprocessing_procedure_ <<- list()
                                   performance_evaluator_ <<- PerformanceEvaluator$new()
                                   hypothesis_tester_ <<- HypothesisTester$new()
                                   conf_level_ <<- 0.95
                                   callSuper(...)
                                   .self
                                 }
                               )

)