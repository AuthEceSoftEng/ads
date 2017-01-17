##' A class responsible for orchestrating the machine learning experiment.
##'
##' @import methods
##' @exportClass Server
##' @export Server
Server <- setRefClass(Class = "Server",
                           fields = list(
                             directories_ = "list",
                             data_prepare_ = "DataPrepare",
                             file_manipulator_ = "FileManipulator",
                             mf2_extractor_ = "mf2Extractor",
                             expert_ = "Expert",
                             optimizer_ = "Optimizer",
                             ensembler_ = "Ensembler"
                           ),
                           methods = list(
                             getDirectory = function(field, ...) {
                               'Returns directory of interest.'
                               return(directories_[field])
                             },
                             createProject = function(dataset_name, project_name, workspace_dir ) {
                               'Creates the directory of current project'
                               # get ADS_workspace full-path from log.xml (or ADS.R)
                               # create project directory 
                               mainDir <- workspace_dir
                               directories_ <<- list( Workspace = workspace_dir)
                               str(directories_)
                               # if project_name is NULL give name of dataset
                               dataset_name_csv <- dataset_name
                               dataset_name <- substr(dataset_name_csv, start =2, stop = nchar(dataset_name) -4 )
                               if( is.null(project_name)) project_name <- dataset_name 
                               subDir <- paste("project", project_name, sep = "_")
                               project_dir <- file.path(mainDir, subDir)
                               directories_$Project <<- project_dir
                               str(directories_)
                               subdirs_list <- list("features/datasets", "features/data_visualization",
                                                    "model/model_files", "model/specifications",
                                                    "testing/statistical_log", "testing/statistical_tests"
                               )
                               #attach project name as parent folder
                               subdirs_list <- paste(project_dir, subdirs_list, sep = "/")
                               #create subdirectories
                               lapply(subdirs_list, dir.create, recursive = TRUE)
                             },
                             performExperiment = function(dataset_name) {
                               'Performs experiment. This is the main function, delegating responsibilities and monitoring the procedure.'
                               # -------- dataset preparation --------
                               # load dataset
                               file_manipulator_$setDirectories(directories = directories_)
                               dataset <- file_manipulator_$loadDataset(dataset_name)
                               # preprocess dataset
                               dictionary <- file_manipulator_$loadOrderedDictionary()
                               dataset <- data_prepare_$convertAttributeTypes(dataset, dictionary)
                               algorithms <-  c(SvmClassifier$new(), AnnClassifier$new(),
                                                KnnClassifier$new(), BayesClassifier$new(),
                                                TreeClassifier$new())
                               task <- list()
                               # create training and testing partitions
                               partitions <- data_prepare_$partitionData(dataset, technique = list(name = "holdout", ratio = 0.9))
                               for(i in seq(1, ncol(partitions))) {
                                 train_indexes <- partitions[,i]
                                 train_dataset <- dataset[train_indexes, ]
                                 test_dataset <- dataset[-train_indexes, ]
                                 stored_processed_datasets <- list()
                                 stored_opt_parameters <- list()
                                 stored_classifiers<- list()
                                 # -------- preprocess, tune and train a model for each classifier --------
                                 for(classifier in algorithms) {
                                   algorithm <- list(algorithm = class(classifier)[1])
                                   task$algorithm <- algorithm
                                   preprocessed_dataset <- expert_$choosePreprocessing(train_dataset, task)
                                   model_name <- classifier$getModelName()
                                   # keep processed dataset, name of model and tuning for later use
                                   stored_processed_datasets[[model_name]]<- preprocessed_dataset
                                   stored_classifiers[[model_name]] <- classifier
                                   # find metafeatures for parameter tuning
                                   # there is the possibility of different metafeatures for each algorithm
                                   metafeature_dataset <- mf2_extractor_$get2MetaFeatures(dataset)
                                   # predict optimal hyperparameters
                                   opt_params <- optimizer_$optimizeHParam(metafeature_dataset, algorithm = algorithm)
                                   stored_opt_parameters[[model_name]] <- opt_params
                                   # train optimized model by calling each classifier's trainModel
                                   val_partitions <- data_prepare_$partitionData(preprocessed_dataset, technique = list(name = "holdout", ratio = 0.9))
                                   training_dataset <- preprocessed_dataset[val_partitions[,1], ]
                                   ensemble_test_dataset <- preprocessed_dataset[-val_partitions[,1], ]
                                   model <- classifier$trainModel(training_dataset = training_dataset, parameters = opt_params, project_dir = directories_$Project)
                                }
                                  # tune ensemble
                                  ensemble_models <- ensembler_$ensemble(classifier = algorithms[[1]], test_dataset = ensemble_test_dataset)
                                  # re-train ensemble_models
                                  models_for_testing <- list()
                                  final_datasets <- list()
                                  for(k in seq(1,length(ensemble_models))) {
                                  # get algorithm's name
                                    selected_model <- ensemble_models[[k]]
                                    model_name <- selected_model$method
                                  # retrieve opt_params and preprocessed, which have already been computed
                                    processed_dataset <- stored_processed_datasets[[model_name]]
                                    opt_param <- stored_opt_parameters[[model_name]]
                                    ensemble_classifier <- stored_classifiers[[model_name]]
                                  # train model of ensemble
                                    model <- ensemble_classifier$trainModel(training_dataset = processed_dataset, parameters = opt_param, project_dir = directories_$Project)
                                    final_datasets[[k]] <- processed_dataset 
                                    models_for_testing[[k]] <- model
                                  }
                                  # get ensemble predictions
                                  predictions <- ensembler_$getEnsemblePredictions(models = models_for_testing, datasets = final_datasets)
                                  # report ensemble's performance
                                }
                                # re-train ensemble's models on whole dataset for storing
                                final_models <- list()
                                final_datasets <- list()
                                for(k in seq(1,length(ensemble_models))) {
                                  # get algorithm's name
                                  selected_model <- ensemble_models[[k]]
                                  model_name <- selected_model$method
                                  # retrieve opt_params and preprocessed, which have already been computed
                                  processed_dataset <- stored_processed_datasets[[model_name]]
                                  opt_param <- stored_opt_parameters[[model_name]]
                                  ensemble_classifier <- stored_classifiers[[model_name]]
                                  # train model of ensemble
                                  model <- ensemble_classifier$trainModel(training_dataset = processed_dataset, parameters = opt_param, project_dir = directories_$Project)
                                  final_datasets[[k]] <- processed_dataset 
                                  final_models[[k]] <- model
                               }
                               # get ensemble predictions
                               final_predictions <- ensembler_$getEnsemblePredictions(models = final_models, datasets = final_datasets)
                               # save trained ensemble
                               file_manipulator_$saveEnsemble(included_models = final_models, predictions = final_predictions)
                             },
                             trainScientist = function() {
                               
                             },
                             initialize=function(...) {
                               # store absolute paths for the moment(to be changed)
                               directories_ <<- list()
                               file_manipulator_ <<- FileManipulator$new(directories_ = directories_)
                               expert_ <<- Expert$new()
                               data_prepare_ <<- DataPrepare$new()
                               mf2_extractor_ <<- mf2Extractor$new()
                               optimizer_ <<- Optimizer$new()
                               ensembler_ <<- Ensembler$new()
                               callSuper(...)
                               .self
                             }
                           )
)
