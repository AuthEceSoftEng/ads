
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
                                   'Converts a string from the user to commands.'
                                   dataset_file_ <<- options$dataset
                                   project_name_ <<- options$project
                                   server_$createProject( dataset_name = dataset_file_,
                                                          project_name = project_name_,
                                                          workspace_dir = options$workspace)
                                   if(options$train) server_$trainScientist()
                                   else server_$performExperiment(dataset_name = dataset_file_)
                                 },
                              initialize=function(...) {
                                   server_ <<- Server$new()
                                   callSuper(...)
                                   .self
                                 }
                               )
)
