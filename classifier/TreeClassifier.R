##' A class responsible for training a tree classifier
##' 
##' @include GenericClassifier.R 
##' @import methods
##' @export
TreeClassifier <- setRefClass(Class = "TreeClassifier",
                             fields = list(

                             ),
                             contains = "GenericClassifier"
)

TreeClassifier$methods(
  trainModel = function(training_dataset, parameters, project_dir) {
    'Train a classification model.'
    optParameters <- expand.grid( maxdepth = c(parameters$maxdepth),
			           nu = c(parameters$nu),
                                  iter = c(parameters$iter)
    )
    
    #train model
    trained_model <- train(Class ~ ., data = training_dataset,
                           method = "ada", 
                           tuneGrid = optParameters,
                           trControl=trainControl(method="none")
    )
    #save model 
    model_dir <- file.path(project_dir, "model/model_files")
    model_file <- file.path(model_dir, "tree_model.rds")
    print(model_file)
    saveRDS(trained_model, model_file)
    return(trained_model)
  },
  initialize=function(...) {

    callSuper(...)
  }
)
