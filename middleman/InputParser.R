##' A class responsible for initiating the experiment, by parsing the user's command and delegating 
##' responsibilities to a server object.
##'
##' @include Server.R
##' @import methods
##' @exportClass InputParser
##' @export InputParser
InputParser <- setRefClass(Class = "InputParser",
                               fields = list(
                                 dataset_file_ = "character",
                                 project_name_ = "character",
                                 server_ = "Server"
                               ),
                               methods = list(
                                 parseCommand = function(options, ...) {
                                   'Converts the command-line arguments provided by a user to commands for Server.'
                                   dataset_file_ <<- options$dataset
                                   if(is.null(options$project)) {
                                      dataset_name  <- substr(dataset_file_, start = 1, stop = nchar(dataset_file_) -4 )
                                      options$project <- dataset_name
                                   }      
                                   project_name_ <<- options$project
                                   server_$createProject( dataset_name = dataset_file_,
                                                          project_name = project_name_,
                                                          workspace_dir = options$workspace)
                                   if(options$train) server_$trainScientist()
                                   else server_$performExperiment(dataset_name = dataset_file_)
                                   if(options$compare) server_$compareAlgorithms()
                                 },
                              initialize=function(...) {
                                   server_ <<- Server$new()
                                   callSuper(...)
                                   .self
                                 }
                               )
)
