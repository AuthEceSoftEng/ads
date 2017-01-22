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
                                   classes_ = "factor",
                                   test_dataset_ = "data.frame",
                                   class_attribute_ = "factor"
                                 ),
                                 methods = list(
                                   evaluateModelContribution = function(models_to_eval){
                                   'Evaluate contribution of a model to the ensemble as improvement to current accuracy'
                                     sampled_accuracies <- list()
                                     for ( i in c(1:length(models_to_eval))) {
                                       model_to_acc <- models_to_eval[[i]]
                                       accuracy <- classifier_$calculateAccuracy(model_to_acc, test_dataset = test_dataset_, class_attribute = class_attribute_)
                                       sampled_accuracies[[i]] <- accuracy
                                   }
                                   contributions <- unlist(sampled_accuracies) - accuracy_
                                   cat("evaluated model contribution")
                                   return(contributions)
                                 },
                                   initializeEnsemble = function(...){
                                     'Initializes the ensemble with N best ranking models'
                                     N <- floor(perc_initial_ * classifier_$getNumModels())
                                     #str(N)
                                     #rank models by its contribution
                                     models <- classifier_$getModels()
                                     #str(models)
                                     contributions <- evaluateModelContribution(models_to_eval = models)
                                     #cat(contributions)
                                     # #pick N best models
                                      initList <- order(contributions, decreasing = TRUE)[1:N]
                                      cat(initList)
                                      init_models <- classifier_$getModels()
                                      init_models <- init_models[initList]
                                      for(i in init_models) {
                                        cat("calling update")
                                        updateEnsemble(i)
                                        cat(length(included_models_))
                                      }
                                   },
                                   getAccuracy = function(...){
                                     'Returns current accuracy of ensemble'
                                     return(accuracy_)
                                   },
                                   getIncludedModels = function(...){
                                     'Return models included in ensemble'
                                     return(included_models_)
                                   },
                                   ensemble = function(classifier, test_dataset,  ...){
                                     'Generates an ensemble of classification models'
                                     test_dataset_  <<- test_dataset
                                     classifier_ <<- classifier
                                     class_attribute_ <<- test_dataset_$Class
                                     test_dataset_$Class <<- NULL
                                     #print(classifier_)
                                     # initialize ensemble probabilities
                                     number_instances <- nrow(test_dataset_)
                                     probabilities_ <<- data.frame( Negative = rep(0, number_instances), Positive =rep(0,number_instances), stringsAsFactors = FALSE)
                                     # initialize ensemble with best-performing models
                                     initializeEnsemble()
                                     cat(length(included_models_))
                                     # str(to_include)
                                      #str(included_models_)
                                      total_models <- classifier_$getModels() 
                                      cat("total models are")
                                      cat(length(total_models))
                                      #  for each bootstrap sample
                                     for ( i in c(1:M_)) {
                                       i
                                       # get sample of all models
                                       models <- sample( x = total_models, replace = TRUE, size = 2)
                                       #cat(length(models))
                                       #str(models)
                                       # evaluate contributions
                                       contributions <- evaluateModelContribution(models_to_eval = models)
                                       #cat(contributions)
                                       # choose mode with best contribution
                                       new_model_index <- which.max(contributions)
                                       new_model <- models[[new_model_index]]
                                       #cat(new_model$method)
                                       #print(new_model)
                                       # update ensemble
                                       updateEnsemble(model = new_model)
                                       
                                     }
                                      cat(length(included_models_))
                                      return(included_models_)
                                     
                                   },
                                   getEnsemblePredictions = function(models, datasets, type = "prob", ...) {
                                     'Returns binary predictions for each model of list'
                                     model_probabilities <- list()
                                     sum_negative <- rep(0,nrow(datasets[[1]]))
                                     sum_positive <- rep(0,nrow(datasets[[1]]))
                                     # brute-force way to initialize sum_model_probabilities and probabilities

                                     sum_model_probabilities <- classifier_$predictClassifier(models[[1]], dataset = datasets[[1]], type = "prob")
                                     cat("after predict")
                                     sum_model_probabilities$Negative <- rep(0,nrow(datasets[[1]]))
                                     sum_model_probabilities$Positive <- rep(0,nrow(datasets[[1]]))
                                     probabilities <- classifier_$predictClassifier(models[[1]], dataset = datasets[[1]], type = "prob")
                                     probabilities$Negative <- rep(0,nrow(datasets[[1]]))
                                     probabilities$Positive <- rep(0,nrow(datasets[[1]]))
                                     for(k in seq(1,length(models))) {
                                       model <- models[[k]]
                                       model_probabilities[[k]] <- classifier_$predictClassifier(model, dataset = datasets[[k]], type = "prob")
                                       #str(model_probabilities[[k]])
                                       #str(model_probabilities[[k]]$Negative)
                                       sum_negative <- model_probabilities[[k]]$Negative + sum_negative
                                       sum_positive <- model_probabilities[[k]]$Positive + sum_positive
                                       sum_model_probabilities$Negative <- model_probabilities[[k]]$Negative + sum_model_probabilities$Negative
                                       sum_model_probabilities$Positive <- model_probabilities[[k]]$Positive + sum_model_probabilities$Positive
                                       #str(sum_negative)
                                      #str(model_probabilities[[k]] )
                                     }
                                     if(type == "prob") {
                                       probabilities$Negative <- sum_model_probabilities$Negative/length(models)
                                       probabilities$Positive <- sum_model_probabilities$Positive/length(models)
                                       return(probabilities)
                                     } else {
                                       probabilities_negative <- sum_negative/length(models)
                                       indexes <- which(probabilities_negative > 0.5)
                                       predictions <- seq(1, length(probabilities_negative))
                                       predictions[indexes] <- 0
                                       predictions[-indexes] <- 1
                                       predictions <- factor(predictions, levels = c(0,1), labels = c("Negative","Positive"))
                                       return(predictions)
                                     } 
                                     
                                   },
                                   updateEnsemble = function(model){
                                     'Incorporates a new model to the ensemble by averaging its predictions.'
                                     cat("updating")
                                     # use model to predict dataset
                                     model_probabilities <- classifier_$predictClassifier(model, dataset = test_dataset_, type = "prob")
                                     # update predictions of ensemble 
                                     probabilities_ <<- (probabilities_ * num_models_ + model_probabilities)/
                                       (num_models_ + 1)
                                     num_models_ <<- num_models_ + 1
                                     # update models included in ensemble (I don't mind including the same mode again)
                                     #str(model$method)
                                     included_models_[[num_models_]] <<- model
                                     indexes <- which((probabilities_$Negative > 0.5))
                                     predictions <- seq(1, nrow(probabilities_))
                                     predictions[indexes] <- 0
                                     predictions[-indexes] <- 1
                                     predictions <- factor(predictions, levels = c(0,1), labels = c("Negative","Positive"))
                                     cm <- confusionMatrix(predictions, class_attribute_)
                                     accuracy_ <<- as.numeric(cm$overall['Accuracy'])
                                     # update ensemble's accuracy
                                   },
                                   compressEnsemble = function(){
                                     'Converts an ensemble to a deep neural network (size and time compression)'
                                   },
                                   initialize=function(...) {
                                     # !!!initialize probabilities_ as data.frames with labels known from classifier_!!!
                                     test_dataset_ <<- data.frame()
                                     num_models_ <<- 0
                                     accuracy_ <<- 0
                                     perc_initial_ <<- 0.5
                                     M_ <<- 5
                                     p_ <<- 0.5
                                     classifier_ <<- GenericClassifier$new()
                                     probabilities_ <<- data.frame(Negative=c(), Positive =c())
                                     callSuper(...)
                                     .self
                                   }
                                 )
)