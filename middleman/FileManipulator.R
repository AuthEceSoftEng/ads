##' A class responsible for saving to and reading from files (.csv, .png, .xml)
##'
##' @include Server.R
##' @import methods
##' @export FileManipulator
##' @exportClass FileManipulator
FileManipulator <- setRefClass(Class = "FileManipulator",
                                 fields = list(
                                   directories_ = "list"
                                 ),
                                 methods = list(
                                   loadHppModel = function(name, ...) {
                                     'Loads a model for predicting hyperparameters'
                                      workspace_dir   <- directories_$Workspace
                                      hpp_models_path <- "HPP/models"
                                      model_file      <- paste(workspace_dir, hpp_models_path, name, sep = "/")
                                      load(model_file)
                                      return(model)
                                   },
                                   loadModelInfo = function(name, ...) {
                                     'Loads information about a model for predicting hyperparameters as a data.frame'
                                     workspace_dir   <- directories_$Workspace
                                     hpp_models_path <- "HPP/models"
                                     info_file      <- paste(workspace_dir, hpp_models_path, name, sep = "/")
                                     info           <- read.csv(info_file,
                                                                 header = TRUE, sep=",", stringsAsFactors=FALSE)
                                     return(info)
                                   },
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
                                   loadResults = function(task, ...) {
                                     'Loads results of algorithms'
                                     if(task$compare$techniques == "models") {
                                       project_dir  <- directories_$Project
                                       results_path <- paste(project_dir, "model/specifications/results", sep = "/")
                                     }
                                     else if (task$techniques == "algorithms")
                                     {
                                       # if implemented, results of different optimization algorithms should be stored in specific directory
                                       #project_dir <- directories_$Project
                                       #results_path <- paste(project_dir, "model/specifications/results", sep = "/")
                                       #cat(results_path)
                                     }
                                     # get files in directory 'project_name'/model/specifications/results
                                     results_files <- paste(results_path, list.files(path = results_path),sep = "/")
                                     # load all files and return data.frames
                                     total_results <- list()
                                     for(file in results_files) {
                                       results                    <- read.csv(file, header = TRUE, sep=",", stringsAsFactors=FALSE)
                                       file_name                  <- deparse(substitute(file))
                                       total_results[[file_name]] <- results
                                     }
                                     names(total_results) <- as.vector(results_files)
                                     return(total_results)
                                   },
                                   clearModels = function(models_to_remove,...) {
                                     'Clears models in directory model/model_files of current project'
                                     project_dir      <- directories_$Project
                                     # create path to file
                                     models_path      <- paste(project_dir, "model/model_files", sep = "/" )
                                     #str(paste(models_path, models_to_remove, sep = "/"))
                                     models_to_delete <- as.vector(models_to_remove)
                                     unlink(models_to_delete)
                                   },
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
                                   saveModel = function(model, model_name, ...) {
                                     'Saves a trained machine learning model'
                                     model_file <- model_name
                                     parameters <- model$finalModel$tuneValue
                                     for(j in seq(1, length(parameters))) {
                                       model_file <- paste(model_file,names(parameters[j]), parameters[[j]] , sep = "_")
                                     }
                                     model_file  <- paste(model_file, "model.Rdata", sep = "_")
                                     project_dir <- directories_$Project
                                     # create path to file
                                     current_path <- getwd()
                                     setwd(project_dir)
                                     model_path  <- paste("model/model_files", model_file, sep = "/")
                                     save(model, file = model_path)
                                     setwd(current_path)
                                     return(model_file)
                                   },
                                   saveEnsemble = function(included_models, info,  ...) {
                                     'Saves built ensemble'
                                     # save all models
                                     models_info <- list()
                                     for(i in 1:length(included_models)) {
                                       models_info[[included_models[[i]]$method]] <- saveModel(model = included_models[[i]], model_name = names(included_models)[i])
                                     }
                                     # save txt with info about ensemble(included models, tuning process, ...)
                                     info_path <- paste(directories_$Project, names(info)[1], sep = "/")
                                     info_path <- paste(info_path, "info.RData", sep = "_")
                                     save(data = info, file = info_path)
                                   },
                                   saveCsv = function(dataset, directory, ...){
                                     'Saves a data.frame into a .csv file.'
                                     # create path to file
                                     file_name <- paste(directories_$Workspace, directory, sep = "/")
                                     # save dataset
                                     write.csv(dataset, file = file_name, row.names = FALSE)
                                   },
                                   # saveXml = function(procedure, name) {
                                   #   'Saves in "name.xml" the procedure list, after translating it to an XmlNode.'
                                   #   # get path to save to  from server
                                   #   # define path+file
                                   #   # save procedure
                                   #   file_name <- paste(directories_$Project,"/", name, ".xml", sep ="")
                                   #   cat(file_name)
                                   #   suppressWarnings(xml <- XML::xmlTree(name))
                                   #   xml$addTag(as.character(substitute(procedure)), close = FALSE)
                                   #   xml_tree <- convertListToXmlTree(xml, procedure, i=1)
                                   #   cat(saveXML(xml_tree$value()))
                                   #   saveXML(xml_tree$value(), file = file_name)
                                   # },
                                   # convertListToXmlTree = function(xml, procedure, i, ...) {
                                   #   'Converts list procedure to an Xml tree'
                                   #   if(length(procedure) >0) {
                                   #       if(is.list(procedure[[i]])) {
                                   #         proc_temp <- procedure[[i]]
                                   #         for(k in seq(1, length(proc_temp))) {
                                   #           xml$addTag(names(proc_temp[k]), close = FALSE)
                                   #             for(j in seq(1, length(proc_temp[[k]]))) {
                                   #               convertListToXmlTree(xml, procedure = proc_temp[[k]], i=j)
                                   #             }
                                   #             xml$closeTag()
                                   #          }
                                   #        } else {
                                   #           xml$addTag(names(procedure[i]), procedure[[i]])
                                   #      }
                                   #   }
                                   #   return(xml) 
                                   # },
                                   saveRdata = function(data, file, ...) {
                                     'Saves data in .RDS formata'
                                     file_name <- paste(directories_$Project, file, sep = "/")
                                     save(x = data, file = file_name)
                                   },
                                   savePng = function(name, plot, ...) {
                                     'Saves a plot to a "name".png.'
                                     # get path to save to from server
                                     # define image path
                                     # open png device
                                     png(filename = image_path)
                                     # plot
                                     # close png device
                                     dev.off()
                                   },
                                   generateReport = function(data, ...) {
                                     'Generates report of experiment with all useful information produced in file experiment.pdf'
                                     # assign list's elements to latex template
                                     cat("Generating report")
                                   },
                                   setDirectories = function(directories, ...) {
                                    directories_ <<- directories                                  
                                   },

                                   initialize=function(...) {
                                     directories_ <<- list( Workspace = "workspace", Project = "workspace/project_my_first_experiment")
                                     callSuper(...)
                                     .self
                                   }
                                 )
)
