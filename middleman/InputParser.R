#' A class responsible for initiating the experiment, by parsing the user's command and delegating 
#' responsibilities to a server object.
#' 
#' 
#' @slot server_ an instance of Class \ref{\link{Server}}
#' 
#' @include Server.R
#' 
#' @import methods
#' @import optparse
#' 
#' @export
InputParser <- setRefClass(Class = "InputParser",
                               fields = list(
                                 server_ = "Server"
                                 )
                           )
InputParser$methods(
  #' Parse command
  #' 
  #' Parses user's input and converts it to commands to \code{\link{Server}}
  #'
  #' @name parseCommand 
  #' @alias parseCommand 
  #' 
  #' @param options command-line options
  parseCommand = function(options, ...) {
    'Converts the command-line arguments provided by a user to commands for Server.'
    dataset_file <- options$dataset
    if(is.null(options$project)) {
      dataset_name    <- substr(dataset_file, start = 1, stop = nchar(dataset_file) - 4)
      options$project <- dataset_name
    }      
    project_name   <- options$project
    experiment_task <- list(compress = options$compress,
                            normalize = options$normalize,
                            inf = options$infinites,
                            outlier = options$outliers,
                            unknown_action = options$unknown_action,
                            unknown_replace = options$unknown_replace,
                            inf_action = options$inf_action,
                            inf_replace = options$inf_replace,
                            dis_svm = options$dis_svm,
                            dis_ann = options$dis_ann,
                            dis_bayes = options$dis_bayes,
                            dis_tree = options$dis_tree,
                            dis_knn = options$dis_knn,
                            ensemble_size = options$ensemble_size,
                            testing_technique = options$testing_technique,
                            performance = options$performance,
                            testing_technique_ratio = options$testing_technique_ratio,
                            benchmark_file = options$benchmark_file,
                            cores = options$cores
                            )
    server_$setExperimentTask(experiment_task = experiment_task)
    if(options$train) {
      server_$trainScientist() # not implemented
    } 
    if(options$experiment) {
      server_$createProject( dataset_name = dataset_file,
                             project_name = project_name,
                             workspace_dir = options$workspace)
      server_$performExperiment(dataset_name = dataset_file)
    } 
    if(options$compare) {
      server_$createProject( dataset_name = dataset_file,
                             project_name = project_name,
                             workspace_dir = options$workspace)
      server_$compareAlgorithms() 
    } 
    if(options$predict) {
      server_$predictDataset(dataset_name = dataset_file, 
                             project_name = project_name,
                             workspace_dir = options$workspace)
    }
  },
  initialize=function(...) {
    server_ <<- Server$new()
    callSuper(...)
    .self
  }
)

