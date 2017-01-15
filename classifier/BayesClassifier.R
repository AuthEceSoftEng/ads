##' A class responsible for training a bayesian classifier
##' 
##' @include GenericClassifier.R 
##' @import methods
##' @export
BayesClassifier <- setRefClass(Class = "BayesClassifier",
                             fields = list(
                               
                             ),
                             contains = "GenericClassifier"
)

BayesClassifier$methods(
  trainModel = function(training_dataset, parameters, project_dir) {
    'Train a classification model.'
    optParameters <- expand.grid( fL = c(parameters$fL),
                                  usekernel = c(parameters$usekernel),
                                  adjust = c(parameters$adjust)
                                  )
    
    #train model
    trained_model <- train(Class ~ ., data = training_dataset,
                           method = "nb", 
                           tuneGrid = optParameters,
                           trControl=trainControl(method="none")
    )
    #save model 
    model_dir <- file.path(project_dir, "model/model_files")
    model_file <- file.path(model_dir, "bayes_model.rds")
    print(model_file)
    saveRDS(trained_model, model_file)
    return(trained_model)
  },
  initialize=function(...) {
    
    callSuper(...)
  }
)
