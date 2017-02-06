##' a generic class, defining the functionality of a classifier
##' 
##' @include GenericClassifier.R
##' @import methods
##' @export
Ensembler <- setRefClass(Class = "Ensembler",
                                 fields = list(
                                   num_models_         = "numeric",
                                   classifier_         = "GenericClassifier",
                                   probabilities_      = "data.frame",
                                   included_models_    = "list",
                                   perc_initial_       = "numeric",
                                   M_                  = "numeric",
                                   p_                  = "numeric",
                                   performance_        = "numeric",
                                   classes_            = "factor",
                                   test_dataset_       = "data.frame",
                                   class_attribute_    = "factor",
                                   performance_metric_ = "character",
                                   expert_             = "Expert",
                                   info_               = "list"
                                 ),
                                 methods = list(
                                   evaluateModelContribution = function(models_to_eval){
                                   'Evaluate contribution of a model to the ensemble as improvement to current performance'
                                     sampled_performances <- list()
                                     for ( i in c(1:length(models_to_eval))) {
                                       model_to_acc              <- models_to_eval[[i]]
                                       # train model to get binary predictions
                                       cat("try to predict")
                                       str(test_dataset_)
                                       predictions               <- classifier_$predictClassifier(model_to_pred = model_to_acc, dataset = test_dataset_, type = "raw" )
                                       predicted_probs           <- classifier_$predictClassifier(model_to_pred = model_to_acc, dataset = test_dataset_, type = "prob")
                                       cat("predicted")
                                       # train model to get prob ability predictions
                                       cat("uevaluate ensembles' s performance")
                                       str(predictions)
                                       cat(length(predictions))
                                       
                                       performance               <- expert_$getPerformance(as.factor(predictions), actual_class = class_attribute_,
                                                                                           predicted_probs= predicted_probs, performance_metric = performance_metric_)
                                       sampled_performances[[i]] <- performance
                                   }
                                   contributions <- unlist(sampled_performances) - performance_
                                   return(contributions)
                                 },
                                   initializeEnsemble = function(project_dir, ...){
                                     'Initializes the ensemble with N best ranking models'
                                     N             <- floor(perc_initial_ * classifier_$getNumModels(project_dir = project_dir))
                                     #str(N)
                                     #rank models by its contribution
                                     models        <- classifier_$getModels(project_dir = project_dir)
                                     #str(models)
                                     contributions <- evaluateModelContribution(models_to_eval = models)
                                     #cat(contributions)
                                     # #pick N best models
                                      initList     <- order(contributions, decreasing = TRUE)[1:N]
                                      init_models  <- classifier_$getModels(project_dir = project_dir)
                                      init_models  <- init_models[initList]
                                      for(i in init_models) {
                                        updateEnsemble(i)
                                      }
                                   },
                                   getPerformance = function(...){
                                     'Returns current accuracy of ensemble'
                                     return(performance_)
                                   },
                                   getIncludedModels = function(...){
                                     'Return models included in ensemble'
                                     return(included_models_)
                                   },
                                   getNumModels = function(...) {
                                     'Returns numbers of models included in ensemble'
                                     return(num_models_)
                                   },
                                   ensemble = function(classifier, test_dataset, performance_metric, project_dir,  ...){
                                     'Generates an ensemble of classification models'
                                     performance_metric_ <<- performance_metric
                                     test_dataset_       <<- test_dataset
                                     classifier_         <<- classifier
                                     class_attribute_    <<- test_dataset_$Class
                                     test_dataset_$Class <<- NULL
                                     # initialize ensemble probabilities
                                     number_instances    <- nrow(test_dataset_)
                                     probabilities_      <<- data.frame( Negative = rep(0, number_instances), Positive =rep(0,number_instances), stringsAsFactors = FALSE)
                                     # initialize ensemble with best-performing models
                                     initializeEnsemble(project_dir = project_dir)
                                     total_models <- classifier_$getModels(project_dir = project_dir) 
                                     # update info of ensemble
                                     # ATTENTION: CONNECT p_ WITH SAMPLE 
                                     size_of_sample  <- ceiling(p_ * length(total_models))
                                     #  for each bootstrap sample
                                     for ( i in c(1:M_)) {
                                       # get sample of all models
                                       
                                       models          <- sample( x = total_models, replace = TRUE, size = size_of_sample)
                                       # evaluate contributions
                                       contributions   <- evaluateModelContribution(models_to_eval = models)
                                       # choose mode with best contribution
                                       new_model_index <- which.max(contributions)
                                       new_model       <- models[[new_model_index]]
                                       # update ensemble
                                       updateEnsemble(model = new_model)
                                     }
                                      info_$parameters <<- list( initial_ensemble_size = M_, probability_of_inclusion = p_, size = length(included_models_))
                                      info_$tuning     <<- list(size = nrow(test_dataset))
                                      return(included_models_)
                                   },
                                   getEnsemblePredictions = function(models, datasets, type = "prob", ...) {
                                     'Returns  predictions for each model of list'
                                     model_probabilities     <- list()
                                     sum_negative            <- rep(0,nrow(datasets[[1]]))
                                     sum_positive            <- rep(0,nrow(datasets[[1]]))
                                     # brute-force way to initialize sum_model_probabilities and probabilities
                                     #sum_model_probabilities          <- classifier_$predictClassifier(models[[1]], dataset = datasets[[1]], type = "prob")
                                     # sum_model_probabilities$Negative <- rep(0,nrow(datasets[[1]]))
                                     # sum_model_probabilities$Positive <- rep(0,nrow(datasets[[1]]))
                                     # probabilities                    <- classifier_$predictClassifier(models[[1]], dataset = datasets[[1]], type = "prob")
                                     # probabilities$Negative           <- rep(0,nrow(datasets[[1]]))
                                     # probabilities$Positive           <- rep(0,nrow(datasets[[1]]))
                                     sum_model_probabilities <- list()
                                     for(k in seq(1,length(models))) {
                                       datasets[[k]]$Class <- NULL
                                       model <- models[[k]]
                                       cat("try to predict in ensemble")
                                       model_probabilities[[k]] <- classifier_$predictClassifier(model, dataset = datasets[[k]], type = "prob")
                                       cat("predicted")
                                       model_probs              <- (model_probabilities[[k]])[[1]]
                                       sum_negative             <- model_probs + sum_negative
                                       sum_positive             <- model_probs + sum_positive

                                     }
                                     sum_model_probabilities$Negative <- sum_negative$Negative/length(models)
                                     sum_model_probabilities$Positive <- sum_positive$Positive/length(models)
                                     if(type == "prob") {
                                       return(sum_model_probabilities)
                                     } else {
                                       probabilities_negative <- sum_model_probabilities$Negative
                                       indexes                <- which(probabilities_negative > 0.5)
                                       predictions            <- seq(1, length(probabilities_negative))
                                       predictions[indexes]   <- 0
                                       predictions[-indexes]  <- 1
                                       predictions            <- factor(predictions, levels = c(0,1), labels = c("Negative","Positive"))
                                       return(predictions)
                                     } 
                                     
                                   },
                                   updateEnsemble = function(model){
                                     'Incorporates a new model to the ensemble by averaging its predictions.'
                                     # use model to predict dataset
                                     model_probabilities             <- classifier_$predictClassifier(model, dataset = test_dataset_, type = "prob")
                                     # update predictions of ensemble 
                                     probabilities_                  <<- (probabilities_ * num_models_ + model_probabilities)/
                                                                            (num_models_ + 1)
                                     num_models_                     <<- num_models_ + 1
                                     # update models included in ensemble (I don't mind including the same mode again)
                                     included_models_[[num_models_]] <<- model
                                     indexes                <- which(probabilities_$Negative > 0.5)
                                     predictions            <- seq(1, length(probabilities_$Negative))
                                     predictions[indexes]   <- 0
                                     predictions[-indexes]  <- 1
                                     predictions            <- factor(predictions, levels = c(0,1), labels = c("Negative","Positive"))
                                     cat("update ensembles' s performance")
                                     str(predictions)
                                     performance_                    <<- expert_$getPerformance(predictions = predictions , actual_class = class_attribute_,
                                                                             predicted_probs = probabilities_, performance_metric = performance_metric_)
                                   },
                                   getInfo = function(...) {
                                     'Returns information about ensemble'
                                     return(info_)
                                   },
                                   savePlots = function(...) {
                                     'Saves plots describing ensembler'
                                     # create plot with performance at each ensemble update
                                     # save plots
                                   },
                                   compressEnsemble = function(){
                                     'Converts an ensemble to a deep neural network (size and time compression)'
                                   },
                                   initialize=function(...) {
                                     # !!!initialize probabilities_ as data.frames with labels known from classifier_!!!
                                     test_dataset_  <<- data.frame()
                                     num_models_    <<- 0
                                     perc_initial_  <<- 0.5
                                     M_             <<- 5
                                     p_             <<- 0.5
                                     classifier_    <<- GenericClassifier$new()
                                     expert_        <<- Expert$new()
                                     probabilities_ <<- data.frame(Negative=c(), Positive =c())
                                     performance_   <<- 0
                                     info_          <<- list()
                                     callSuper(...)
                                     .self
                                   }
                                 )
)