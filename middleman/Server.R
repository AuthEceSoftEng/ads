#' A class responsible for orchestrating the machine learning experiment.
#' 
#' Its responsibilities include building project's subdirectories, performing the experiment
#' and saving all experiment's output information.
#'  
#' @slot directories_ list of project's subdirectories
#' @slot data_prepare_ an instance of Class \ref{\link{DataPrepare}}
#' @slot file_manipulator_ an instance of Class \ref{\link{FileManipulator}}
#' @slot mf2_extractor_ an instance of Class \ref{\link{mf2Extractor}}
#' @slot optimizer_ an instance of Class \ref{\link{Optimizer}}
#' @slot ensemble_ an instance of Class \ref{\link{Ensembler}}
#' @slot experiment_task_ list of information on how to conduct the experiment
#' @slot expert_ an instance of Class \ref{\link{Expert}}
#' @slot performance_visualizer_ an instance of Class \ref{\link{PerformanceVisualizer}}
#' @slot feature_visualizer_ an instance of Class \ref{\link{FeatureVisualizer}}
#' @slot performance_metric_ desired metric to evaluate performance
#' @slot testing_technique_
#' 
#' @include DataPrepare.R
#' @include mf2Extractor.R
#' @include Optimizer.R
#' @include Ensembler.R
#' @include PerformanceVisualizer.R
#' @include FeatureVisualizer.R
#' @include Expert.R
#' @include FileManipulator.R
#'  
#' 
#' @export


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
                        performance_visualizer_ = "PerformanceVisualizer",
                        feature_visualizer_     = "FeatureVisualizer",
                        performance_metric_     = "character",
                        testing_technique_      = "list",
                        dataset_name_           = "character"
                        )
                      )

Server$methods(
  #' Create project
  #' 
  #' Creates project's file structure and initiates experiment.
  #'  
  #' @name createProject
  #' @alias createProject
  #' 
  #' @param dataset_name name of dataset
  #' @param project_name name of project
  #' @param workspace_dir directory of workspace
  createProject = function(dataset_name, project_name, workspace_dir ) {
    'Creates the directory of current project'
    # create project directory 
    mainDir              <- workspace_dir
    directories_         <<- list( Workspace = workspace_dir)
    # if project_name is NULL give name of dataset
    subDir               <- paste("project", project_name, sep = "_")
    # add timestamp 
    subDir               <- paste(subDir, format(Sys.time(), "%y_%m_%d_%H_%M_%S"), sep = "_")
    project_dir          <- file.path(mainDir, subDir)
    directories_$Project <<- project_dir
    subdirs_list         <- list("features/datasets", "features/data_visualization",
                                 "model/model_files", "model/specifications/results",
                                 "testing/statistical_log", "testing/statistical_tests"
    )
    dataset_name_        <<- dataset_name
    # attach project name as parent folder
    subdirs_list         <- paste(project_dir, subdirs_list, sep = "/")
    # create subdirectories
    lapply(subdirs_list, dir.create, recursive = TRUE)
  },
  #' Perform experiment
  #' 
  #' Performs a machine learning experiment. An optimal ensemble for a binary classification dataset
  #' is trained, evaluated and information upon the experiment is provided under current project. This function
  #' orchestrates the experiment by delegating responsibilities to the other packages.
  #'  
  #' @name performExperiment
  #' @alias performExperiment
  #' 
  #' @param dataset_name name of dataset
  performExperiment = function(dataset_name) {
    'Performs experiment. This is the main function, delegating responsibilities and monitoring the procedure.'
    # -------- preparation --------
    # load dataset
    file_manipulator_$setDirectories(directories = directories_)
    dataset <- file_manipulator_$loadDataset(dataset_name)
    # preprocess dataset
    dictionary <- file_manipulator_$loadOrderedDictionary()
    dataset    <- data_prepare_$convertAttributeTypes(dataset, dictionary)
    # define machine learning algortihms used in ensemble
    algorithms <- c()
    if(!(experiment_task_$dis_knn)) {
      algorithms <- c(algorithms, KnnClassifier$new())
    }
    if(!experiment_task_$dis_tree) {
      algorithms <- c(algorithms, TreeClassifier$new())
    }
    if(!experiment_task_$dis_ann) {
      algorithms <- c(algorithms, AnnClassifier$new())
    }
    if(!experiment_task_$dis_svm) {
      algorithms <- c(algorithms, SvmClassifier$new())
    }
    if(!experiment_task_$dis_bayes) {
      algorithms <- c(algorithms, BayesClassifier$new())
    }
    # --- create training and testing partitions ---
    # heuristically define size of testing partition in case user has not requested one
    if(!is.na(experiment_task_$testing_technique_)) {
      testing_technique_$name <<-experiment_task_$testing_technique
    }
    if(!is.na(experiment_task_$testing_technique_ratio)) {
      testing_technique_$ratio <<-experiment_task_$testing_technique_ratio
    }
    if(is.na(testing_technique_$ratio)) {
      partition_expert          <- Expert$new()
      testing_technique_$ratio  <<- partition_expert$getPartitionRatio(testing_technique = testing_technique_,
                                                                       N = nrow(dataset))
    }
    partitions            <- data_prepare_$partitionData(dataset, technique = testing_technique_)
    # -------- begin processing of folds --------
    ensemble_models_total <- c()
    ensemble_performance  <- list()
    roc_pred              <- c()
    #setup parallel backend to use many processors
    total_dir    <- "total"
    registerDoParallel(experiment_task_$cores)
    fold_results <-    foreach(i=seq(1, ncol(partitions))) %dopar% {
      # create directory of current fold
      fold_dir     <- file.path(directories_$Project, "model/model_files")
      dir.create(file.path(fold_dir, "total")) 
      current_fold <- paste("fold", i, sep ="_") 
      fold_dir     <- file.path(fold_dir, current_fold)
      dir.create(fold_dir)
      train_indexes   <- partitions[,i]
      train_dataset   <- dataset[train_indexes, ]
      testing_dataset <- dataset[-train_indexes, ]
      # advice expert for right performance metric
      if(is.na(experiment_task_$performance)) {
        performance_expert        <- Expert$new()
        performance_metric_       <<- performance_expert$choosePerformanceMetric(model_name = model_name, dataset = train_dataset)
      } else {
        performance_metric_     <<- experiment_task_$performance
      }
      stored_processed_datasets <- list()
      stored_opt_parameters     <- list()
      stored_classifiers        <- list()
      stored_experts            <- list()
      # -------- preprocess, tune and train a model for each classifier --------
      for(classifier in algorithms) {
        expert               <- Expert$new()
        algorithm            <- list(algorithm = class(classifier)[1], parameters = classifier$getModelParameters())
        experiment_task_$algorithm       <<- algorithm
        preprocessed_dataset <- expert$choosePreprocessing(train_dataset, task = experiment_task_)
        model_name           <- classifier$getModelName()
        # keep processed dataset, name of model and tuning for later use
        stored_processed_datasets[[model_name]] <- preprocessed_dataset
        stored_classifiers[[model_name]]        <- classifier
        stored_experts[[model_name]]            <- expert
        # find metafeatures for parameter tuning
        metafeatures <- mf2_extractor_$get2MetaFeatures(preprocessed_dataset)
        # predict optimal hyperparameters
        opt_params <- optimizer_$optimizeHParam(algorithm  = algorithm$algorithm,
                                                  parameters = algorithm$parameters, metafeatures = metafeatures)
        stored_opt_parameters[[model_name]] <- opt_params
        # divide training dataset into training and tuning partitions
        val_partitions        <- data_prepare_$partitionData(dataset = preprocessed_dataset, technique = list(name = "holdout", ratio = 0.8))
        training_dataset      <- preprocessed_dataset[val_partitions[,1], ]
        ensemble_test_dataset <- preprocessed_dataset[-val_partitions[,1], ]
        # train optimized model by calling each classifier's trainModel and save it under project's directory
        classifier$trainModel(training_dataset = training_dataset, parameters = opt_params,
                              file_manipulator = file_manipulator_, current_fold = current_fold)
      }
      # tune ensemble
      if(!is.na(experiment_task_$ensemble_size)) ensembler_$setM(M = experiment_task_$ensemble_size)
      ensemble_models <- ensembler_$ensemble(classifier = algorithms[[1]], test_dataset = ensemble_test_dataset,
                                             performance_metric = performance_metric_, project_dir = directories_$Project,
                                             current_fold = current_fold)
      # -------- re-train ensemble_models on training partition --------
      total_models       <- classifier$getModels(project_dir = directories_$Project, current_fold = current_fold)
      unused_models      <- setdiff(total_models, ensemble_models)
      file_manipulator_$clearModels(models_to_remove = unused_models, current_fold = current_fold)
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
        ensemble_classifier <- stored_classifiers[[model_name]]
        # train model of ensemble
        rm(model)
        model_files         <- ensemble_classifier$trainModel(training_dataset = processed_dataset, parameters = opt_param,
                                                                      file_manipulator = file_manipulator_, current_fold = total_dir
                                                              )
        final_datasets[[k]]     <- processed_dataset
        current_expert          <- stored_experts[[model_name]]
        current_test_dataset    <- current_expert$applyPreprocessing(dataset = testing_dataset)
        test_datasets[[k]]      <- current_test_dataset
        model_files             <- model_files[[1]]
        model                   <- model_files
        model_expert            <- Expert$new()
        model_classifier        <- GenericClassifier$new()
        predictions             <- model_classifier$predictModel(model_to_pred = model,
                                                                      dataset = current_test_dataset , type = "raw")
        predicted_probabilities <- model_classifier$predictModel(model_to_pred = model,
                                                                      dataset = current_test_dataset, type = "prob")
        model_performance       <- model_expert$getPerformance(predictions = as.factor(predictions),
                                                               actual_class = current_test_dataset$Class, 
                                                               predicted_probs = predicted_probabilities,
                                                               performance_metric = performance_metric_)
        model_performances      <- c(model_performances, model_performance)
      }
      # -------- get performance on testing partition  --------
      # get ensemble predictions
      ensembler               <- Ensembler$new()
      predictions             <- ensembler$getEnsemblePredictions(datasets = test_datasets, type = "raw",
                                                                  project_dir = directories_$Project, current_fold = current_fold)
      predicted_probabilities <- ensembler$getEnsemblePredictions(dataset = test_datasets, type = "prob",
                                                                  project_dir = directories_$Project, current_fold= current_fold)
      # report ensemble's performance
      ensemble_expert           <- Expert$new()
      ensemble_expert$processTask(task = list())
      roc_pred                  <- c(roc_pred, prediction(predictions = predicted_probabilities$Positive,
                                                     labels = test_datasets[[1]]$Class))
      ensemble_performance[[i]] <- ensemble_expert$getPerformance(predictions = as.factor(predictions),
                                                                  actual_class = test_datasets[[1]]$Class,
                                                                  predicted_probs = predicted_probabilities,
                                                                  performance_metric = performance_metric_)
      ensemble_models_total     <- c(ensemble_models_total, unlist(ensemble_models))
      file_manipulator_$clearModels(current_fold = current_fold)
      return(list(ensemble_models_total = ensemble_models_total,stored_experts = stored_experts,
                  stored_classifiers =stored_classifiers, ensemble_performance = ensemble_performance, roc_pred =roc_pred,
                  ensemble_expert =  ensemble_expert, metafeatures = metafeatures, train_dataset = train_dataset,
                  model_performances = model_performances))
    }
    fold_results <- fold_results[[1]]
    # re-train ensemble's models on whole dataset for storing
    final_models   <- list()
    expert_tasks   <- list()
    counter <- 1
    for(k in seq(1,length(fold_results$ensemble_models_total))) {
      # get algorithm's name
      selected_model <- fold_results$ensemble_models_total[k]
      load(selected_model)
      model_name     <- model$method
      # retrieve opt_params and preprocessed, which have already been computed
      current_expert      <- fold_results$stored_experts[[model_name]]
      processed_task      <- current_expert$getProcessedTask()
      expert_tasks[[k]]   <- processed_task 
      processed_dataset   <- current_expert$choosePreprocessing(fold_results$train_dataset, task = processed_task, final = TRUE)
      opt_param           <- as.list(model$bestTune)
      ensemble_classifier <- fold_results$stored_classifiers[[model_name]]
      # train models of ensemble
      model_files         <- ensemble_classifier$trainModel(training_dataset = processed_dataset, parameters = opt_param,
                                                                  file_manipulator = file_manipulator_, current_fold = total_dir)
      models <- list()
      # append testing performance to each model
      for(m in 1:length(model_files)) {
        model <- model_files[[m]]
        load(model)
        model$performance_metric <- fold_results$model_performances[k]
        models[[m]] <- model
      }
      lapply(models, function(x) file_manipulator_$saveModel(model = x, model_name = x$method, current_fold = total_dir ))
    }
    # save all useful information produced during the experiment
    saveExperimentInfo(performance = fold_results$ensemble_performance, roc_pred = fold_results$roc_pred,
                       experts = fold_results$stored_experts, ensemble_expert = fold_results$ensemble_expert,
                       stored_classifiers =fold_results$stored_classifiers, expert_tasks = expert_tasks,
                       metafeatures = fold_results$metafeatures)
  },
  #' Save experiment's information
  #' 
  #' Saves useful information produced during the experiment in project's experiment_info.Rdata 
  #' and orders automl-report generation.
  #'  
  #' @name saveExperimentInfo
  #' @alias saveExperimentInfo
  #' 
  #' @param performance performance of final ensemble
  #' @param roc_pred 
  #' @param experts expert of each algorithm
  #' @param ensemble_expert expert of ensemble
  #' @param stored_classifiers clasifier of each algorithm
  saveExperimentInfo = function(performance, roc_pred, experts, ensemble_expert, stored_classifiers, expert_tasks, 
                                metafeatures, ...) {
    'Saves information about ensemble, individual models, pipeline of experiment and plots'
    # gather RData of experiment
    experiment_info <- gatherExperimentInfo(classifiers = stored_classifiers, experts,  performance = performance,
                                            roc_pred = roc_pred, ensemble_expert = ensemble_expert,
                                            expert_tasks = expert_tasks,
                                            metafeatures = metafeatures)
    file_manipulator_$saveRdata(data = experiment_info, file = "experiment_info.Rdata")
    file_manipulator_$generateReport(data = experiment_info, type = "experiment")
  },
  #' Gather experiment's information
  #' 
  #' Gathers useful information produced during the experiment.
  #' This includes preprocessing procedure, machine learning algorithms, final models, ensemble
  #' characteristics and testing technique.
  #'  
  #' @name gatherExperimentInfo
  #' @alias gatherExperimentInfo
  #' 
  #' @param classifiers clasifier of each algorithm
  #' @param experts expert of each algorithm
  #' @param performance performance of ensemble in each fold
  #' @param roc_pred predictions to make a ROC curve
  #' @param ensemble_expert expert of ensemble
  gatherExperimentInfo = function(classifiers, experts, performance, roc_pred, ensemble_expert, expert_tasks, 
                                  metafeatures, ...) {
    experiment_info                     <- list()
    # gather metafeatures of dataset
    experiment_info$metafeatures        <- metafeatures
    # gather name of dataset
    experiment_info$dataset_name        <- dataset_name_
    # gather anticipation metric
    experiment_info$anticipation_metric <- mf2_extractor_$getAnticipationMetric()
    # gather data preparation info
    experiment_info$data_prepare        <- data_prepare_$getInfo()
    # gather preprocessing info 
    for(i in 1:length(experts)) {
      data_compressor  <- experts[[i]]$getDataCompressor()
      normalizer       <- experts[[i]]$getNormalizer()
      feature_engineer <- experts[[i]]$getFeatureEngineer()
      inap_remover     <-  experts[[i]]$getInapRemover()
      preprocess       <- list(data_compression = data_compressor$getInfo(), normalization = normalizer$getInfo(),
                               feature_engineering = feature_engineer$getInfo(), removing_inappropriate = inap_remover$getInfo(),
                               classifiers[[i]]$getInfo())
      experiment_info[[paste("algorithm_", i, sep = "")]] <- list(preprocess = preprocess)
    }
    classifier      <- GenericClassifier$new()
    included_models <- classifier$getModels(project_dir = directories_$Project, current_fold = "total")
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
      model_name                    <- paste(model_name, "model.Rdata", sep = "_")
      model_info[["name"]]          <- model_name
      parameters                    <- as.list(model$bestTune)
      parameters                    <- lapply(parameters, function(x) as.vector(x))
      model_info[["parameters"]]    <- parameters
      model_info[["adequate_info"]] <- model$adequate_info
      model_info[["performance"]]   <- model$performance_metric
      experiment_info[[paste("model_", i, sep = "")]] <- list(model_info = model_info)
    }
    # gather hyperparameter prediction info
    experiment_info$hyperparameter_info  <- optimizer_$getInfo()
    # gather ensemble info
    experiment_info$ensemble             <- ensembler_$getInfo()
    experiment_info$ensemble$performance <- performance
    experiment_info$ensemble$roc_pred    <- roc_pred
    # gather info about testing
    performance_evaluator       <- ensemble_expert$getPerformanceEvaluator()
    metrics                     <- performance_evaluator$getInfo()
    experiment_info$testing     <- list(model_validation = list(metrics = metrics,
                                                                technique = testing_technique_ ))
    # gather info for predicting
    experiment_info$task_for_prediction <- expert_tasks 
    # info about installation directory
    experiment_info$install_dir <- getwd()
    return(experiment_info)
  },
  #' Compare algorithms
  #'
  #' Loads benchmarks, containing the performance of different
  #' algorithms on different datasets and compares them.
  #' 
  #' @name compareAlgorithms
  #' @alias compareAlgorithms
  compareAlgorithms = function(...) {
    'Regulates the process of comparing diffenent algorithms'
    # load algorithms' results
    file_manipulator_$setDirectories(directories = directories_)
    benchmarks <- file_manipulator_$loadBenchmarks( file = experiment_task_$benchmark_file)
    # call Expert's compareModels
    expert <- Expert$new()
    technique_evaluation <- expert$compareModels(results = benchmarks, task = experiment_task_)
    # store results info 
    #hypothesis_tester    <- expert$getHypothesisTester()
    #technique_evaluation <- hypothesis_tester$getInfo()
    technique_evaluation$install_dir <- getwd()
    technique_evaluation$results     <- benchmarks
    technique_evaluation$p_value     <- technique_evaluation$p.value 
    file_manipulator_$saveRdata(technique_evaluation, "comparison_info.Rdata")
    file_manipulator_$generateReport(data = technique_evaluation, type = "compare")
  },
  #' Predict dataset
  #'
  #' Predicts class of input dataset using trained ensemble of models under define project. If Class
  #' is included in input dataset 
  #' 
  #' @name compareAlgorithms
  #' @alias compareAlgorithms
  #'
  #' @param dataset_name name of dataset
  #' @param project_name name of project
  #' 
  #' @return 
  predictDataset = function(dataset_name, project_name, workspace_dir, ...) {
    'Regulates the process of comparing diffenent algorithms'
    # check if project exists and is completed
    experiment_info_file <- file.path(workspace_dir, project_name, "experiment_info.Rdata")
    if(!file.exists(experiment_info_file)) {
      cat("ERROR: project does not exist or is not completed.")
    } else {
      # load dataset
      file_manipulator_$setDirectories(directories = list(Workspace = workspace_dir))
      dataset <- file_manipulator_$loadDataset(dataset_name)
      # preprocess dataset
      dictionary <- file_manipulator_$loadOrderedDictionary()
      dataset    <- data_prepare_$convertAttributeTypes(dataset, dictionary)
      dataset$Class <- NULL
      load(experiment_info_file)
      task       <- data$task_for_prediction
      datasets   <- list()
      for(i in seq(1, length(task))) {
        expert_$setProcessedTask(task[[i]])
        datasets[[i]]    <- expert_$applyPreprocessing(dataset = dataset)
      }
      # get predictions of ensemble
      ensembler               <- Ensembler$new()
      predictions             <- ensembler$getEnsemblePredictions(datasets = datasets, type = "raw",
                                                                  project_dir = file.path(workspace_dir,project_name),
                                                                  current_fold = "total")
      # save predictions file
      write.csv(as.data.frame(predictions), file = file.path(workspace_dir, project_name, "predictions.csv"),
                row.names = FALSE)
      }
  },
  #' Set experiment's task
  #'
  #' @name setExperimentTask
  #' @alias setExperimentTask
  #' 
  #' @param experiment_task list of experiment information
  setExperimentTask = function(experiment_task, ...) {
    experiment_task_ <<- experiment_task
  },
  #' Get directory
  #'
  #' @name getDirectory
  #' @alias getDirectory
  #' 
  #' @param field type of directory
  #' 
  #' @return path of directory
  getDirectory = function(field, ...) {
    'Returns directory of interest.'
    return(directories_[field])
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
    testing_technique_      <<- list(name = "kfold", ratio = 0.8)
    dataset_name_           <<- ""
    callSuper(...)
    .self
  }
)                      
