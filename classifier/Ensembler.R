#' A class responsible for building and manipulating an ensemble of models.
#'
#' Ensembler offers the functionalities of buiding an ensemble and getting predictions from it.
#' 
#' @slot num_models_ number of current models in ensemble
#' @slot classifier_ an instance of Class \code{\link{GenericClassifier}}
#' @slot probabiities_ ensemble's predicted probabilities
#' @slot included_models_ names of models included in ensemble
#' @slot perc_initial_ percentage of initial models
#' @slot M_ total number of models in final ensemble
#' @slot p_ probability of inclusion in bootstrap sample
#' @slot performance_ current ensemble performancce
#' @slot test_dataset_ dataset to perform validation on
#' @slot class_attribute_ class
#' @slot performance_metric_ desired metric for performance evaluation
#' @slot expert_ an instance of Class \code{\link{Expert}}
#' @slot evolution_ vector of consecutive performances of ensemble
#' @slot seed_ seed for control of randomness
#' @slot info_ list of information about ensemble
#' 
#' @include GenericClassifier.R 
#' @include FileManipulator.R
#' @include Expert.R
#' 
#' @import methods
#' @import caret
#' @export
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
                                   test_dataset_       = "data.frame",
                                   class_attribute_    = "factor",
                                   performance_metric_ = "character",
                                   expert_             = "Expert",
                                   evolution_          = "numeric",
                                   seed_               = "numeric",
                                   info_               = "list"
                                 )
                         )

Ensembler$methods(
  #' Build ensemble
  #' 
  #' The building of a forward-model-selection ensemble is performed as described in
  #'  \url{http://www.cs.cornell.edu/~alexn/papers/shotgun.icml04.revised.rev2.pdf}.
  #' 
  #' @name ensemble
  #' @alias ensemble
  #' 
  #' @param classifier an instance of \code{\link{GenericClassifier}}
  #' @param test_dataset dataset to perform validation on
  #' @param performance_metric desired metric for performance evaluation
  #' @param project_dir directory of current project
  #' 
  #' @return names of files containing modes included in ensemble
  ensemble = function(classifier, test_dataset, performance_metric, project_dir, current_fold,  ...){
    'Generates an ensemble of classification models'
    # keep information necessary for rest of Ensembler's work
    performance_metric_ <<- performance_metric
    test_dataset_       <<- test_dataset
    classifier_         <<- classifier
    class_attribute_    <<- test_dataset_$Class
    test_dataset_$Class <<- NULL
    # initialize ensemble probabilities
    number_instances    <- nrow(test_dataset_)
    probabilities_      <<- data.frame( Negative = rep(0, number_instances), Positive =rep(0,number_instances), stringsAsFactors = FALSE)
    # initialize ensemble with best-performing models
    initializeEnsemble(project_dir = project_dir, current_fold)
    total_models <- classifier_$getModels(project_dir = project_dir, current_fold) 
    # find size of bootstrap sample
    size_of_sample  <- ceiling(p_ * length(total_models))
    evolution_ <<- c(0)
    p_inclusion <- size_of_sample/length(total_models)
    info_$parameters <<- list( initial_ensemble_size = length(included_models_), probability_of_inclusion = p_inclusion ,
                               size = 0)
    # begin bootstrap ensemble updates
    for (i in seq(1,(M_-length(included_models_)))) {
      # get bootstrap sample of total models
      set.seed(seed_)
      models          <- sample( x = total_models, replace = TRUE, size = size_of_sample)
      # evaluate contributions
      contributions   <- evaluateModelContribution(models_to_eval = models)
      # choose model with best contribution
      new_model_index <- which.max(contributions)
      new_model       <- models[[new_model_index]]
      # update ensemble
      updateEnsemble(model = new_model)
    }
    # update info of ensemble
    info_$parameters$size <<- length(included_models_)
    info_$tuning          <<- list(size = nrow(test_dataset))
    info_$evolution       <<- evolution_
    return(included_models_)
  },
  #' Initialize ensemble
  #' 
  #' Initializes ensemble with best-performing models, the number of which comes from \code{"perc_initial"}. 
  #' This is a technique to avoid overtraining in large libraries of models.
  #' 
  #' @name initializeEnsemble
  #' @alias initializeEnsemble
  #' 
  #' @param project_dir directory of current project
  #' 
  #' @return names of files containing saved models
  initializeEnsemble = function(project_dir, current_fold, ...){
    'Initializes the ensemble with N best ranking models'
    N             <- floor(perc_initial_ * classifier_$getNumModels(project_dir = project_dir, current_fold))
    if(N > M_) { # in case initial size exceeds ensemble size
      N <- M_
    }
    # fill initial ensemble
    init_models <- list()
    if(N!=0) {
      # get all models
      models        <- classifier_$getModels(project_dir = project_dir, current_fold)
      # get all models' contributions
      contributions <- evaluateModelContribution(models_to_eval = models)
      # pick N best models
      initList     <- order(contributions, decreasing = TRUE)[1:N]
      init_models  <- classifier_$getModels(project_dir = project_dir, current_fold)
      init_models  <- init_models[initList]
      # update ensemble with N best models
      for(i in init_models) {
        updateEnsemble(i)
      }  
    }
    return(init_models)
  },
  #' Evaluate contribution of model
  #' 
  #' Evaluate's a model's contribution to the ensemble as its effect on the performance.
  #' 
  #' @name evaluateModelContribution
  #' @alias  evaluateModelContribution
  #' 
  #' @param models_to_eval a list of models for evaluation
  #' 
  #' @return  vector of contributions
  evaluateModelContribution = function(models_to_eval){
    'Evaluate contribution of a model to the ensemble as improvement to current performance.'
     sampled_performances <- list()
     for (i in c(1:length(models_to_eval))) {
       model_to_acc              <- models_to_eval[[i]]
       # train model to get binary predictions
       predictions               <- classifier_$predictModel(model_to_pred = model_to_acc, dataset = test_dataset_, type = "raw" )
       # train model to get probability predictions
       predicted_probs           <- classifier_$predictModel(model_to_pred = model_to_acc, dataset = test_dataset_, type = "prob")
       performance               <- expert_$getPerformance(as.factor(predictions), actual_class = class_attribute_,
                                                           predicted_probs= predicted_probs, performance_metric = performance_metric_)
       sampled_performances[[i]] <- performance
     }
     # calculate contribution of each model
     contributions <- unlist(sampled_performances) - performance_
     return(contributions)
  },
  #' Update ensemble
  #' 
  #' Updates ensemble by including a new model. Inclusion consists in appending it to \code{"included_models_"}
  #' and updating ensemble's performance. The evolution of the ensemble is kept for monitoring.
  #' 
  #' @name updateEnsemble
  #' @alias  updateEnsemble
  #' 
  #' @param model model to be included
  updateEnsemble = function(model){
    'Incorporates a new model to the ensemble by averaging its predictions.'
    # use model to predict dataset
    model_probabilities             <- classifier_$predictModel(model, dataset = test_dataset_, type = "prob")
    # update predictions of ensemble 
    probabilities_                  <<- (probabilities_ * num_models_ + model_probabilities)/
      (num_models_ + 1)
    num_models_                     <<- num_models_ + 1
    # convert probabilistic to raw predictions
    included_models_[[num_models_]] <<- model
    indexes                <- which(probabilities_$Negative > 0.5)
    predictions            <- seq(1, length(probabilities_$Negative))
    predictions[indexes]   <- 0
    predictions[-indexes]  <- 1
    predictions            <- factor(predictions, levels = c(0,1), labels = c("Negative","Positive"))
    # update performance
    performance_           <<- expert_$getPerformance(predictions = predictions , actual_class = class_attribute_,
                                                               predicted_probs = probabilities_, performance_metric = performance_metric_)
    # update evolution
    evolution_             <<- c(evolution_,performance_)
  },
  #' Get predictions of ensemble
  #' 
  #' Get probabilistic or raw predictions from ensemble. It is assumed that \code{"project_dir"} includes only selected models
  #' 
  #' @name getEnsemblePredictions
  #' @alias  getEnsemblePredictions
  #' 
  #' @param datasets preprocessed dataset for each model
  #' @param type "prob" or "raw"
  #' @param project_dir directory of current project
  #' 
  #' @return  a list of predictions
  getEnsemblePredictions = function(datasets, type = "prob", project_dir, current_fold,...) {
    'Returns  predictions for each model of list.'
    # get total models
    models <- classifier_$getModels(project_dir = project_dir, current_fold)
    # get predicted probabilities
    sum_negative            <- rep(0,nrow(datasets[[1]]))
    sum_positive            <- rep(0,nrow(datasets[[1]]))
    for(k in seq(1,length(models))) {
      datasets[[k]]$Class <- NULL
      model <- models[[k]]
      model_probs <- classifier_$predictModel(model, dataset = datasets[[k]], type = "prob")
      sum_negative             <- model_probs[[1]] + sum_negative
      sum_positive             <- model_probs[[2]] + sum_positive
    }
    sum_model_probabilities <- list(Negative = sum_negative/length(models), Positive =  sum_positive/length(models))
    if(type == "prob") {
      return(sum_model_probabilities)
    } else {
      # convert probabilities to binary predictions
      probabilities_negative <- sum_model_probabilities$Negative
      indexes                <- which(probabilities_negative > 0.5)
      predictions            <- seq(1, length(probabilities_negative))
      predictions[indexes]   <- 0
      predictions[-indexes]  <- 1
      predictions            <- factor(predictions, levels = c(0,1), labels = c("Negative","Positive"))
      return(predictions)
    }
  },
  #' Get best performing model in a library of models
  #' 
  #' A utility function used to detect the best performing model in a library of models. 
  #' Used to evaluate the ensemble, but not build it.
  #' 
  #' @name  getBest
  #' @alias   getBest
  #' 
  #' @param classifier an instance of \code{\link{GenericClassifier}}
  #' @param test_dataset dataset used for validation of performance
  #' @param performance_metric desired metric for performance evaluation
  #' @param project_dir directory of current project
  #' 
  #' @return  a list of predictions
  getBest = function(classifier, test_dataset, performance_metric, project_dir,  ...){
    'Returns best performing model in library of models'
    performance_metric_ <<- performance_metric
    test_dataset_       <<- test_dataset
    classifier_         <<- classifier
    class_attribute_    <<- test_dataset_$Class
    test_dataset_$Class <<- NULL
    # initialize ensemble probabilities
    number_instances    <- nrow(test_dataset_)
    probabilities_      <<- data.frame( Negative = rep(0, number_instances), Positive =rep(0,number_instances), stringsAsFactors = FALSE)
    # initialize ensemble with best-performing models
    perc_initial_  <<- 1/(classifier_$getNumModels(project_dir = project_dir, current_fold))
    best_models <- initializeEnsemble(project_dir = project_dir, current_fold)
    return(best_models[[1]])
  },
  #' Return performance of ensemble
  #' 
  #' 
  #' @name getPerformance
  #' @alias getPerformance
  #' 
  #' 
  #' @return performance
  getPerformance = function(...){
    'Returns current performance of ensemble'
    return(performance_)
  },
  #' Return models included in ensemble
  #' 
  #' 
  #' @name getIncludedModels
  #' @alias getIncludedModels
  #' 
  #' 
  #' @return names of models included in ensemble
  getIncludedModels = function(...){
    'Return models included in ensemble'
    return(included_models_)
  },
  #' Return number of models included in ensemble
  #' 
  #' 
  #' @name getNumModels
  #' @alias getNumModels
  #' 
  #' 
  #' @return names of models included in ensemble
  getNumModels = function(...) {
    'Returns numbers of models included in ensemble'
     return(num_models_)
  },
  #' Return information about the ensemble
  #' 
  #' Information includes tuning parameters and evolution of ensemble.
  #' 
  #' @name getInfo
  #' @alias getInfo
  #' 
  #' 
  #' @return list of information
  getInfo = function(...) {
    'Returns information about ensemble.'
    return(info_)
  },
  #' Set M
  #' 
  #' @name setM
  #' @alias setM
  setM = function(M, ...) {
    'Returns information about ensemble.'
    M_ <<- M
  },
  #' Compress ensemble into a deep neural network
  #' 
  #' Information includes tuning parameters and evolution of ensemble.
  #' 
  #' @name compressEnsemble
  #' @alias  compressEnsemble
  #' 
  #' 
  #' @return compressed ensemble
  compressEnsemble = function(){
    'Converts an ensemble to a deep neural network (size and time compression).'
    # to be implemented if needed
  },
  initialize=function(...) {
    test_dataset_  <<- data.frame()
    num_models_    <<- 0
    perc_initial_  <<- 0.01
    M_             <<- 50
    p_             <<- 0.5
    classifier_    <<- GenericClassifier$new()
    expert_        <<- Expert$new()
    probabilities_ <<- data.frame(Negative=c(), Positive =c())
    performance_   <<- 0
    info_          <<- list()
    evolution_     <<- c(0)
    seed_          <<- 1
    callSuper(...)
    .self
  }
)
