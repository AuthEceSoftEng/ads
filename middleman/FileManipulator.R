#' A class responsible for saving to and reading from files.
#' 
#' Supported file formats are CSV, PNG and XML).
#' 
#' @slot directories_ list of directories useful to experiment
#' 
#' @import knitr
#' @import markdown
#' 
#' @export
FileManipulator <- setRefClass(Class = "FileManipulator",
                               fields = list(
                                 directories_ = "list"
                                 )
                               )
FileManipulator$methods(
  #' Load HPP model
  #' 
  #' Loads HyperParameterPrediction model for hyperparameters optimization.
  #'
  #' @name loadHppModel
  #' @alias loadHppModel 
  #' 
  #' @param name name of model
  loadHppModel = function(name, ...) {
    'Loads a model for predicting hyperparameters'
    workspace_dir   <- directories_$Workspace
    hpp_models_path <- "HPP/models"
    model_file      <- paste(workspace_dir, hpp_models_path, name, sep = "/")
    load(model_file)
    return(model)
  },
  #' Load HPP model info
  #' 
  #' Loads information about HyperParameterPrediction model 
  #' 
  #' @name loadHPPModelInfo
  #' @alias loadHPPModelInfo 
  #' 
  #' @param name name of model
  loadHPPModelInfo = function(name, ...) {
    'Loads information about a model for predicting hyperparameters as a data.frame'
    workspace_dir   <- directories_$Workspace
    hpp_models_path <- "HPP/models"
    info_file       <- paste(workspace_dir, hpp_models_path, name, sep = "/")
    info            <- read.csv(info_file,
                               header = TRUE, sep=",", stringsAsFactors=FALSE)
    return(info)
  },
  #' Load dataset
  #' 
  #' Loads a csv file under [workspace/datasets_repo](../workspace/datasets_repo)
  #' 
  #' @name loadDataset
  #' @alias loadDataset 
  #' 
  #' @param name name of dataset
  loadDataset = function(name, ...) {
    'Loads file specified by name in repository with datasets into a data.frame'
    # get directory of datasets
    workspace_dir <- directories_$Workspace
    # create path to file
    dataset_path  <- paste(workspace_dir, "datasets_repo", name, sep = "/" )
    # read file into data.frame
    dataset       <- read.csv(dataset_path,
                              header = TRUE, sep=",", stringsAsFactors=FALSE)
    return(dataset)
  },
  #' Load benchmarks
  #' 
  #' Loads results of completed experiments, paced under [workspace/benchmarks] (workspace/benchmarks). 
  #' Used to compare the performance of different algorithms on different datasets.
  #'  
  #' @name loadBenchmarks
  #' @alias loadBenchmarks
  loadBenchmarks = function(file, ...) {
    'Loads results of algorithms'
    if(is.na(file)) {
      benchmark_file <- "thesis_benchmarks.csv"
    } else {
      benchmark_file <- file
    }
    benchmark_file <- file.path("benchmarks", benchmark_file)
    benchmark_file <- file.path(directories_$Workspace, benchmark_file)
    benchmarks     <- read.csv(benchmark_file,
                              header = TRUE, sep=",", stringsAsFactors=FALSE
                              )
    return(benchmarks)
  },
  #' Clear models
  #' 
  #' Deletes specified models under project' [model/model_files].
  #'  
  #' @name clearModels
  #' @alias clearModels
  #' 
  #' @param models_to_remove names of models to remove
  clearModels = function(models_to_remove = NULL, current_fold, ...) {
    'Clears models in directory model/model_files of current project'
    project_dir      <- directories_$Project
    # create path to file
    models_path      <- paste(project_dir, "model/model_files", sep = "/" )
    models_path      <- file.path(models_path, current_fold)
    if(is.null(models_to_remove)) {
      models_to_delete <- list.files(path = models_path)
    } else {
      models_to_delete <- as.vector(models_to_remove)
    }
    unlink(models_to_delete)
  },
  #' Load ordered dictionary
  #' 
  #' Loads information about ordered categorical attributes from file
  #'  [workspace/heuristics/dictionary.csv] (workspace/heuristics/dictionary.csv)
  #'  
  #' @name loadOrderedDictionary
  #' @alias loadOrderedDictionary
  #' 
  #' @return data.frame of information
  loadOrderedDictionary = function(...) {
    'Loads dictionary of words indicating ordered features.'
    # get ADS Workspace
    workspace_dir <- directories_$Workspace
    # create path to file
    file_path     <- file.path(workspace_dir, "heuristics_repo/ordered_dictionary.csv")
    # load file
    dic           <- read.csv(file_path, header = TRUE, sep=",", stringsAsFactors=FALSE)
    return(dic)
  },
  #' Save model
  #' 
  #' Saves a trained machine model under project's [model/model/files] in.Rdata
  #' format with variable's name "model" .
  #'  
  #' @name saveModel
  #' @alias saveModel
  #' 
  #' @return model path
  saveModel = function(model, model_name, current_fold, ...) {
    'Saves a trained machine learning model'
    model_file <- model_name
    parameters <- model$bestTune
    for(j in seq(1, length(parameters))) {
      model_file <- paste(model_file,names(parameters[j]), parameters[[j]] , sep = "_")
    }
    model_file  <- paste(model_file, "model.RData", sep = "_")
    project_dir <- directories_$Project
    # create path to file
    model_path  <- paste(project_dir,"model/model_files", current_fold, model_file, sep = "/")
    save(model, file = model_path)
    return(model_path)
  },
  #' Save csv file
  #' 
  #' Saves a data.frame in a csv fle under [workspace] (workspace) .
  #'  
  #' @name saveCsv
  #' @alias saveCsv
  #' 
  #' @return data.frame of information
  saveCsv = function(dataset, directory, ...){
    'Saves a data.frame into a .csv file.'
    # create path to file
    file_name <- paste(directories_$Workspace, directory, sep = "/")
    # save dataset
    write.csv(dataset, file = file_name, row.names = FALSE)
  },
  #' Load metafeatures of repository
  #' 
  #' Load metafeatures of datasets used for training the HPP models. Also, 
  #' loads information about them useful to draw Cosmos and calculate
  #' anticipation metric.
  #'  
  #' @name loadRepoMetafeatures
  #' @alias loadRepoMetafeatures
  #' 
  #' @return list of metafeatures and parameters
  loadRepoMetafeatures = function(...) {
    'Loads meta-features characterizing the training datasets in repo'
    metafeatures <- list()
    # get workspace
    workspace_dir <- directories_$Workspace
    # create path to dataset file
    dataset_path  <- file.path(workspace_dir, "HPP/metafeatures/training_metafeatures.csv")
    # load dataset file
    metafeatures$dataset   <- read.csv(dataset_path, header = TRUE, sep=",", stringsAsFactors=FALSE)
    metafeatures$dataset$X <- NULL
    # create path to parameters file
    parameters_path <- file.path(workspace_dir, "HPP/metafeatures/metafeatures_parameters.Rdata")
    # load parameters file
    load(parameters_path)
    metafeatures$info <- info   
    return(metafeatures)
  },
  #' Save Rdata
  #' 
  #' Saves a variable in .Rdata format under project's directory.
  #'  
  #' @name saveRdata
  #' @alias saveRdata
  #' 
  #' @param data variable to save
  #' @param file name of file
  saveRdata = function(data, file, ...) {
    'Saves data in .RDS formata'
    file_name <- paste(directories_$Project, file, sep = "/")
    save(x = data, file = file_name)
  },
  #' Generate report
  #' 
  #' Generates experiment's report under project's directory using [docs/automl_report.Rmd] (docs/automl_report.Rmd).
  #'  
  #' @name generateReport
  #' @alias generateReport
  #' 
  #' @param data information about experiment
  #' @parm type "experiment" or "compare"
  generateReport = function(data, type, ...) {
    'Generates report of experiment with all useful information produced in file experiment.pdf'
    # choose report type
    if(type == "experiment") {
      report_name <- "automl_report"
    } else {
      report_name <- "compare_report"
    }
    # copy template to current project
    report_template <- file.path("docs", report_name)
    current_report  <- file.path(directories_$Project, report_name)
    file.copy(from = paste(report_template, ".Rmd", sep =""), to = paste(current_report, ".Rmd", sep ="")) 
    knit(paste(current_report, ".Rmd", sep =""))
    file.rename(from = paste(file.path(data$install_dir, report_name), ".md", sep =""),
                to = paste(current_report, ".md", sep ="")) 
    # copy figures to current project
    figure_dir        <- file.path(data$install_dir, "figure")
    figure_files      <- list.files(path = figure_dir, full.names = FALSE)
    dir.create(file.path(directories_$Project, "figure"))
    destination_files <- file.path(directories_$Project, "figure", figure_files) 
    figure_files      <- list.files(path = figure_dir, full.names = TRUE)
    file.copy(from=figure_files, to=destination_files, 
              copy.mode = TRUE)
    markdownToHTML(paste(current_report, ".md", sep =""),
                   paste(current_report, ".html", sep =""))
  },
  #' Set directories
  #'  
  #' @name setDirectories
  #' @alias setDirectories
  #' 
  #' @param directories directories necessary for experiment
  setDirectories = function(directories, ...) {
    directories_ <<- directories                                  
  },
  initialize=function(...) {
    directories_ <<- list( Workspace = "workspace", Project = "")
    callSuper(...)
    .self
  }
)
