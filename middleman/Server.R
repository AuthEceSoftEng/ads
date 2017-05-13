##' A class responsible for orchestrating the machine learning experiment.
##'
##' @import methods
##' @exportClass Server
##' @export Server
Server <- setRefClass(Class = "Server",
                      fields = list(
                        directories_            = "list",
                        data_prepare_           = "DataPrepare",
                        file_manipulator_       = "FileManipulator",
                        mf2_extractor_          = "mf2Extractor",
                        optimizer_              = "Optimizer",
                        ensembler_              = "Ensembler", 
                        experiment_task_        = "list",
                        expert_                 = "Expert",
                        time_                   = "numeric",
                        performance_visualizer_ = "PerformanceVisualizer",
                        feature_visualizer_     = "FeatureVisualizer",
                        performance_metric_     = "character",
                        testing_technique_      = "list"
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
                          mainDir              <- workspace_dir
                          directories_         <<- list( Workspace = workspace_dir)
                          # if project_name is NULL give name of dataset
                          subDir               <- paste("project", project_name, sep = "_")
                          project_dir          <- file.path(mainDir, subDir)
                          directories_$Project <<- project_dir
                          subdirs_list         <- list("features/datasets", "features/data_visualization",
                                                       "model/model_files", "model/specifications/results",
                                                       "testing/statistical_log", "testing/statistical_tests"
                          )
                          #attach project name as parent folder
                          subdirs_list         <- paste(project_dir, subdirs_list, sep = "/")
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
                          dataset    <- data_prepare_$convertAttributeTypes(dataset, dictionary)
                          # algorithms <-  c(SvmClassifier$new(), AnnClassifier$new(),
                          #                  KnnClassifier$new(), BayesClassifier$new(),
                          #                  TreeClassifier$new())
                          algorithms <- c(KnnClassifier$new(), TreeClassifier$new(), AnnClassifier$new(),SvmClassifier$new())
                          task       <- list()
                          # create training and testing partitions
                          partitions <- data_prepare_$partitionData(dataset, technique = testing_technique_)
                          ensemble_models_total <- list()
                          ensemble_performance <- list()
                          for(i in seq(1, ncol(partitions))) {
                            train_indexes   <- partitions[,i]
                            train_dataset   <- dataset[train_indexes, ]
                            testing_dataset <- dataset[-train_indexes, ]
                            # advice expert for right performance metric
                            performance_expert        <- Expert$new()
                            performance_metric_       <<- performance_expert$choosePerformanceMetric(model_name = model_name, dataset = train_dataset)
                            stored_processed_datasets <- list()
                            stored_opt_parameters     <- list()
                            stored_classifiers        <- list()
                            stored_experts            <- list()
                            # -------- preprocess, tune and train a model for each classifier --------
                            for(classifier in algorithms) {
                              expert               <- Expert$new()
                              algorithm            <- list(algorithm = class(classifier)[1], parameters = classifier$getModelParameters())
                              task$algorithm       <- algorithm
                              preprocessed_dataset <- expert$choosePreprocessing(train_dataset, task)
                              model_name           <- classifier$getModelName()
                              # keep processed dataset, name of model and tuning for later use
                              stored_processed_datasets[[model_name]] <- preprocessed_dataset
                              stored_classifiers[[model_name]]        <- classifier
                              stored_experts[[model_name]]            <- expert
                              # find metafeatures for parameter tuning
                              # there is the possibility of different metafeatures for each algorithm
                              metafeature_dataset <- mf2_extractor_$get2MetaFeatures(preprocessed_dataset)
                              # predict optimal hyperparameters
                              opt_params <- apply(as.data.frame(metafeature_dataset), 1, function(x) {
                                example  <- matrix(x, nrow=1, ncol=ncol(metafeature_dataset), byrow=TRUE)
                                example  <- data.frame(example)
                                colnames(example) = names(metafeature_dataset)
                                opt_params <- optimizer_$optimizeHParam(algorithm  = algorithm$algorithm, parameters = algorithm$parameters, metafeatures = example)})
                              stored_opt_parameters[[model_name]] <- opt_params
                              # train optimized model by calling each classifier's trainModel and save it under project's directory
                              val_partitions        <- data_prepare_$partitionData(preprocessed_dataset, technique = list(name = "holdout", ratio = 0.9))
                              training_dataset      <- preprocessed_dataset[val_partitions[,1], ]
                              ensemble_test_dataset <- preprocessed_dataset[-val_partitions[,1], ]
                              classifier$trainModel(training_dataset = training_dataset, parameters = opt_params,
                                                                             file_manipulator = file_manipulator_)
                            }
                            # tune ensemble
                            ensemble_models <- ensembler_$ensemble(classifier = algorithms[[1]], test_dataset = ensemble_test_dataset,
                                                                   performance_metric = performance_metric_, project_dir = directories_$Project)
                            # re-train ensemble_models
                            total_models <- classifier$getModels(project_dir = directories_$Project)
                            unused_models <- setdiff(total_models, ensemble_models)
                            file_manipulator_$clearModels(models_to_remove = unused_models)
                            models_for_testing <- list()
                            final_datasets     <- list()
                            test_datasets      <- list()
                            model_performances <- c()
                            for(k in seq(1,length(ensemble_models))) {
                              # get algorithm's name
                              selected_model <- ensemble_models[[k]]
                              load(selected_model)
                              model_name     <- model$method
                              # retrieve opt_params and preprocessed, which have already been computed
                              processed_dataset   <- model$trainingData
                              opt_param           <- as.list(model$bestTune)
                              opt_param           <- list(opt_param)
                              ensemble_classifier <- stored_classifiers[[model_name]]
                              # train model of ensemble
                              rm(model)
                              model_files                 <- ensemble_classifier$trainModel(training_dataset = processed_dataset, parameters = opt_param,
                                                                                            file_manipulator = file_manipulator_
                              )
                              #model_files <- lapply(models, function(x) file_manipulator_$saveModel(model = x, model_name = x$method ))
                              final_datasets[[k]]     <- processed_dataset 
                              current_expert          <- stored_experts[[model_name]]
                              current_test_dataset    <- current_expert$applyPreprocessing(dataset = testing_dataset)
                              test_datasets[[k]]      <- current_test_dataset
                              model_files <- model_files[[1]]
                              model <- model_files
                              model_expert     <- Expert$new()
                              model_classifier <- GenericClassifier$new()
                              predictions <- model_classifier$predictClassifier(model_to_pred = model, dataset = current_test_dataset , type = "raw")
                              predicted_probabilities <- model_classifier$predictClassifier(model_to_pred = model, dataset = current_test_dataset, type = "prob")
                              model_performance <- model_expert$getPerformance(predictions = as.factor(predictions), actual_class = current_test_dataset$Class,
                                                                               predicted_probs = predicted_probabilities, performance_metric = performance_metric_)
                              model_performances <- c(model_performances, model_performance)
                            }
                            # APPLY PREPROCESSING ON TEST_DATASET
                            # get ensemble predictions
                            ensembler               <- Ensembler$new()
                            predictions             <- ensembler$getEnsemblePredictions(datasets = test_datasets, type = "raw",
                                                                                        project_dir = directories_$Project)
                            predicted_probabilities <- ensembler$getEnsemblePredictions(dataset = test_datasets, type = "prob",
                                                                                        project_dir = directories_$Project)
                            # report ensemble's performance
                            ensemble_expert      <- Expert$new()
                            ensemble_expert$processTask(task = list())
                            ensemble_performance[[i]] <- ensemble_expert$getPerformance(predictions = as.factor(predictions), actual_class = test_datasets[[1]]$Class,
                                                                                   predicted_probs = predicted_probabilities, performance_metric = performance_metric_)
                            ensemble_models_total <- c(ensemble_models_total, ensemble_models)
                            
                          }
                          # re-train ensemble's models on whole dataset for storing
                          final_models   <- list()
                          final_datasets <- list()
                          counter <- 1
                          for(k in seq(1,length(ensemble_models_total))) {
                            # get algorithm's name
                            selected_model <- ensemble_models_total[[k]]
                            load(selected_model)
                            model_name     <- model$method
                            # retrieve opt_params and preprocessed, which have already been computed
                            # CAUTION: PREPROCESSED DATASETS MUST BE RE-COMPUTED
                            current_expert      <- stored_experts[[model_name]]
                            processed_task      <- current_expert$getProcessedTask()
                            processed_dataset   <- current_expert$choosePreprocessing(train_dataset, task = processed_task, final = TRUE)
                            opt_param           <- as.list(model$bestTune)
                            opt_param           <- list(opt_param)
                            ensemble_classifier <- stored_classifiers[[model_name]]
                            # train model of ensemble
                            model_files               <- ensemble_classifier$trainModel(training_dataset = processed_dataset, parameters = opt_param,
                                                                                        file_manipulator = file_manipulator_)
                            models <- list()
                            for(m in 1:length(model_files)) {
                              model <- model_files[[m]]
                              load(model)
                              model$performance_metric <- model_performances[k]
                              models[[m]] <- model
                            }
                            lapply(models, function(x) file_manipulator_$saveModel(model = x, model_name = x$method ))
                            final_datasets[[k]] <- processed_dataset 
                          }
                          # get ensemble predictions
                          final_predictions <- ensembler_$getEnsemblePredictions( datasets = final_datasets, type = "raw",
                                                                                  project_dir = directories_$Project)
                          # save all useful information produced during the experiment
                          saveExperimentInfo( predictions = final_predictions, performance = ensemble_performance, experts = stored_experts, ensemble_expert = ensemble_expert)
                        },
                        saveExperimentInfo = function(performance, experts, ensemble_expert,  ...) {
                          'Saves information about ensemble, individual models, pipeline of experiment and plots'
                          classifier <- GenericClassifier$new()
                          included_models <- classifier$getModels(project_dir = directories_$Project)
                          # ensemble_info (this list contains all info for ensemble.xml
                          ensemble_info <- list() 
                          # clear models produced during training
                          # save models included in ensemble
                          for(i in seq(1, length(included_models))) {
                            model_info <- list()
                            model      <- included_models[[i]]
                            model      <- model[[1]]
                            load(model)
                            model_name <- model$method
                            parameters <- model$bestTune
                            # fill information of particular model
                            model_info[["name"]] <- model$method
                            # EACH MODEL MUST BE TESTED INDIDUALLY TO ATTACH PERFORMANCE( or do it in ensemble )
                            #model_info[["performance"]] <- model$performance
                            parameters                  <- as.list(model$bestTune)
                            parameters                  <- lapply(parameters, function(x) as.vector(x))
                            model_info[["parameters"]]  <- parameters
                            
                            ensemble_info[[model_name]] <- model_info
                            #file_manipulator_$saveModel(model, model_name)
                          }
                          # gather info about ensemble
                          ensemble_info[["ensemble_data"]] <- list(performance = performance, time = time_)
                          # save RData of ensemble
                          #file_manipulator_$saveRdata(data = ensemble_info, file = "ensemble_info.Rdata")
                          # save RData of experiment
                          experiment_info <- gatherExperimentInfo(experts, included_models = included_models, performance = performance, ensemble_expert = ensemble_expert)
                          file_manipulator_$saveRdata(data = experiment_info, file = "experiment_info.Rdata")
                          # save data-visualization plots
                          #feature_visualizer_$savePlots()
                          # save performance plots
                          #performance_visualizer_$savePlots()
                          # save plots describing ensemble
                          #ensembler_$savePlots()
                          # generate report
                          file_manipulator_$generateReport(data = experiment_info)
                        },
                        gatherExperimentInfo = function(experts, included_models, performance, ensemble_expert,  ...) {
                          experiment_info              <- list()
                          # gather anticipation metric
                          experiment_info$anticipation_metric <- mf2_extractor_$getAnticipationMetric()
                          # gather data preparation info
                          experiment_info$data_prepare <- data_prepare_$getInfo()
                          # gather preprocessing info 
                          for(i in 1:length(experts)) {
                            data_compressor  <- experts[[i]]$getDataCompressor()
                            normalizer       <- experts[[i]]$getNormalizer()
                            feature_engineer <- experts[[i]]$getFeatureEngineer()
                            inap_remover     <-  experts[[i]]$getInapRemover()
                            preprocess       <- list(data_compression = data_compressor$getInfo(), normalization = normalizer$getInfo(),
                                                     feature_engineering = feature_engineer$getInfo(), removing_inappropriate = inap_remover$getInfo())
                            experiment_info[[paste("algorithm_", i, sep = "")]] <- list(preprocess = preprocess)
                          }
                          for(i in 1:length(included_models)) {
                            model_info       <- list()
                            model            <- included_models[[i]]
                            load(model)
                            model_name       <- model$method
                            parameters       <- model$bestTune
                            # fill information of particular model
                            model_info[["method"]] <- model$method
                            for(j in seq(1, length(parameters))) {
                              model_name <- paste(model_name,names(parameters[j]), parameters[[j]] , sep = "_")
                            }
                            model_name           <- paste(model_name, "model.Rdata", sep = "_")
                            model_info[["name"]] <- model_name
                            # EACH MODEL MUST BE TESTED INDIDUALLY TO ATTACH PERFORMANCE( or do it in ensemble )
                            #model_info[["performance"]] <- model$performance
                            parameters                 <- as.list(model$bestTune)
                            parameters                 <- lapply(parameters, function(x) as.vector(x))
                            model_info[["parameters"]] <- parameters
                            model_info[["performance"]] <- model$performance_metric
                            experiment_info[[paste("model_", i, sep = "")]] <- list(model_info = model_info)
                          }
                          # gather ensemble info
                          experiment_info$ensemble             <- ensembler_$getInfo()
                          experiment_info$ensemble$performance <- performance
                          # gather info about testing
                          performance_evaluator   <- ensemble_expert$getPerformanceEvaluator()
                          metrics                 <- performance_evaluator$getInfo()
                          experiment_info$testing <- list(model_validation = list(metrics = metrics, technique = testing_technique_ ))
                          return(experiment_info)
                        },
                        compareAlgorithms = function(...) {
                          'Regulates the process of comparing diffenent algorithms'
                          # load algorithms' results
                          results_tables <- file_manipulator_$loadResults(task = experiment_task_)
                          # call Expert's compareModels
                          expert <- Expert$new()
                          expert$compareModels(results = results_tables, task = experiment_task_)
                          # store results info 
                          hypothesis_tester    <- expert$getHypothesisTester()
                          technique_evaluation <- hypothesis_tester$getInfo()
                        },
                        trainScientist = function() {
                          
                        },
                        initialize=function(...) {
                          # store absolute paths for the moment(to be changed)
                          directories_            <<- list()
                          file_manipulator_       <<- FileManipulator$new(directories_ = directories_)
                          data_prepare_           <<- DataPrepare$new()
                          mf2_extractor_          <<- mf2Extractor$new()
                          optimizer_              <<- Optimizer$new()
                          ensembler_              <<- Ensembler$new()
                          expert_                 <<- Expert$new()
                          experiment_task_        <<- list(compare = list(techniques = "models", metric = "Accuracy"))
                          performance_visualizer_ <<- PerformanceVisualizer$new()
                          feature_visualizer_     <<- FeatureVisualizer$new()
                          performance_metric_     <<- ""
                          testing_technique_      <<- list(name = "kfold", ratio = 0.9)
                          time_ <<- 0
                          callSuper(...)
                          .self
                        }
                      )
)
