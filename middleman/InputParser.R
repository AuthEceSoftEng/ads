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
    experimet_task <- list(preprocess = list(compress = options$compress,
                                             normalize = options$normalize,
                                             unknown = options$unknown,
                                             inf = options$infinites,
                                             outlier = options$outliers
    ),
    algorithms    <- options$algorithms,
    ensemble_size <- options$ensemble_size)
    server_$createProject( dataset_name = dataset_file,
                           project_name = project_name,
                           workspace_dir = options$workspace)
    server_$setExperimentTask(experiment_task = experiment_task)
    if(options$train) server_$trainScientist() # not implemented
    else server_$performExperiment(dataset_name = dataset_file)
    if(options$compare) server_$compareAlgorithms() # not implemented
  },
  initialize=function(...) {
    server_ <<- Server$new()
    callSuper(...)
    .self
  }
)

