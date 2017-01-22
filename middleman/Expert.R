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
                                  performance_evaluator_ = "PerformanceEvaluator"
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
                                   return(FALSE)
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
                                   if(IsLogTransformRequired(dataset)) {
                                     dataset <- feature_engineer_$applyLogTransform(dataset)
                                   }
                                   # store preprocessing procedure in list (for later application on test_dataset)
                                   preprocessing_procedure_ <<- list()
                                   return(dataset)
                                 },
                                 applyPreprocessing = function(dataset, task, ...) {
                                   'Applies preprocessing to dataset based on info in preprocessing_procedure_'
                                   # retrieve info from preprocessing_procedure_ and apply it
                                   #preprocessed_dataset <- dataset
                                   # WARNING: temporary for testing, task should not be an argument
                                   preprocessed_dataset <- choosePreprocessing(dataset, task)
                                   return(preprocessed_dataset)
                                 },
                                 getPerformance = function(predictions, actual_class, predicted_probs, ...) {
                                   'Returns performance of model'
                                   # choose appropriate performance metric based on heuristics
                                   performance <- c()
                                   # --- call Performance Evaluator ---
                                   # get metrics extracted from confusion matrix
                                   performance_evaluator_$setPredictions(predictions = predictions)
                                   performance_evaluator_$setActualClass(actual_class = actual_class)
                                   if("auc" %in% processed_task_$metrics) {
                                     performance <- performance_evaluator_$calculateAUC(predicted_probs = predicted_probs)
                                   } else {
                                     performance <- performance_evaluator_$calculateConfusionMatrixMetrics(selected_metrics = processed_task_$metrics)
                                   }
                                     return(performance)
                                 },
                                 getProcessedTask = function(...) {
                                   return(processed_task_)
                                 }, 
                                 compareTechniques = function(results_a, results_b, ...) {
                                   'Returns comparisons on techniques(machine learning or optimization algorithms) based on hypothesis testing'
                                 },
                                 initialize = function(...) {
                                   feature_engineer_ <<- FeatureEngineer$new()
                                   inap_remover_ <<- InapRemover$new()
                                   normalizer_ <<- Normalizer$new()
                                   data_compressor_ <<- DataCompressor$new()
                                   processed_task_ <<- list(model = NULL , application = NULL, certain_range = NULL, pca_percentage = NULL)
                                   preprocessing_procedure_ <<- list()
                                   performance_evaluator_ <<- PerformanceEvaluator$new()
                                   callSuper(...)
                                   .self
                                 }
                               )

)