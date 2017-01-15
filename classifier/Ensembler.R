##' a generic class, defining the functionality of a classifier
##' 
##' @include GenericClassifier.R
##' @import methods
##' @export
Ensembler <- setRefClass(Class = "Ensembler",
                                 fields = list(
                                   num_models_ = "numeric",
                                   classifier_ = "GenericClassifier",
                                   probabilities_ = "data.frame",
                                   included_models_ = "list",
                                   perc_initial_  = "numeric",
                                   M_ = "numeric",
                                   p_ = "numeric",
                                   accuracy_ = "numeric",
                                   classes_ = "factor"
                                 ),
                                 methods = list(
                                   evaluateModelContribution = function(models_to_eval){
                                   'Evaluate contribution of a model to the ensemble as improvement to current accuracy'
                                     sampled_accuracies <- list()
                                     for ( i in c(1:length(models_to_eval))) {
                                     model_to_acc <- models_to_eval[[i]]
                                     accuracy <- classifier_$calculateAccuracy(model_to_acc)
                                     sampled_accuracies[[i]] <- accuracy
                                   }
                                   contributions <- unlist(sampled_accuracies) - accuracy_
                                   return(contributions)
                                 },
                                   initializeEnsemble = function(...){
                                     'Initializes the ensemble with N best ranking models'
                                     N <- floor(perc_initial_ * classifier_$getNumModels())
                                     #rank models by its contribution
                                     models <- classifier_$getModels()
                                     contributions <- evaluateModelContribution(models_to_eval = models)
                                     #pick N best models
                                     initList <- order(contributions, decreasing = TRUE)[1:N]
                                     num_models_ <<- length(initList)
                                     return(initList)
                                   },
                                   getAccuracy = function(...){
                                     'Returns current accuracy of ensemble'
                                     return(accuracy_)
                                   },
                                   getIncludedModels = function(...){
                                     'Return models included in ensemble'
                                     return(included_models_)
                                   },
                                   findClasses = function(...){
                                     class_attribute <- classifier_$getClass()
                                     classes <- unique(class_attribute[,1])
                                     return(classes)
                                   },
                                   ensemble = function(){
                                     'Generates an ensemble of classification models'
                                     # initialize ensemble with best-performing models
                                     to_include <- initializeEnsemble()
                                     included_models_ <<- classifier_$getModels()
                                     models <- classifier_$getModels()
                                     included_models_ <<- included_models_[to_include]
                                     models <- models[to_include]
                                     # for each bootstrap sample
                                     for ( i in c(1:M_)) {
                                       # get sample of models
                                       models <- sample( x = models, replace = TRUE, size = 2)
                                       # evaluate contributions
                                       contributions <- evaluateModelContribution(models_to_eval = models)
                                       # choose mode with best contribution
                                       new_model_index <- which.max(contributions)
                                       new_model <- models[new_model_index]
                                       # update ensemble
                                       updateEnsemble(model = new_model)
                                     }
                                   },
                                   updateEnsemble = function(model){
                                     'Incorporates a new model to the ensemble by averaging its predictions. '
                                     # use model to predict dataset
                                     model_probabilities <- classifier_$predictClassifier(model, dataset = classifier_$dataset_)
                                     #initialize probabilities
                                     classes_ <<- findClasses()
                                     number_instances <- nrow(classifier_$getDataset())
                                     probabilities_ <<- data.frame( classes1 = rep(0, number_instances), classes2 =rep(0,number_instances), stringsAsFactors = FALSE)
                                     colnames(probabilities_) <<- classes_
                                     # average predictions in ensemble 
                                     probabilities_ <<- (probabilities_ * num_models_ + model_probabilities)/
                                       (num_models_ + 1)
                                     included_models_ <<- list(included_models_, model)
                                     class_attribute <- classifier_$getClass()
                                     predictions <- as.numeric(((probabilities_))>0.5)
                                     class_attribute <- as.numeric(class_attribute[,1])
                                     class_attribute[class_attribute==2] <-0
                                     accuracy_ <<- 1 - sum(abs(class_attribute - predictions)) /length(class_attribute)
                                   },
                                   compressEnsemble = function(){
                                     'Converts an ensemble to a deep neural network'
                                   },
                                   initialize=function(...) {
                                     # !!!initialize probabilities_ as data.frames with labels known from classifier_!!!
                                    
                                     num_models_ <<- 0
                                     accuracy_ <<- 0
                                     perc_initial_ <<- 0.5
                                     M_ <<- 20
                                     p_ <<- 0.5
                                     callSuper(...)
                                     .self
                                   }
                                 )
)