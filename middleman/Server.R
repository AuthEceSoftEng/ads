
##' A class responsible for orchestrating the machine learning experiment.
##'
##' @import methods
##' @exportClass Server
##' @export Server
Server <- setRefClass(Class = "Server",
                           fields = list(
                             directories_ = "character",
                             cleaner_ = "Cleaner",
                             file_manipulator_ ="FileManipulator",
                             expert_ = "Expert",
                             optimizer_ ="Optimizer",
                             generic_classifier_ ="GenericClassifier"
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
                               directories_ <<- setNames(workspace_dir, c("Workspace"))
                               # if project_name is NULL give name of dataset
                               dataset_name_csv <- dataset_name
                               dataset_name <- substr(dataset_name_csv, start =2, stop = nchar(dataset_name) -4 )
                               if( is.null(project_name)) project_name <- dataset_name 
                               subDir <- paste("project", project_name, sep = "_")
                               project_dir_ <- file.path(mainDir, subDir)
                               subdirs_list <- list("features/datasets", "features/data_visualization",
                                                    "model/model_files", "model/specifications",
                                                    "testing/statistical_log", "testing/statistical_tests"
                               )
                               #attach project name as parent folder
                               subdirs_list <- paste(project_dir_, subdirs_list, sep = "/")
                               #create subdirectories
                               lapply(subdirs_list, dir.create, recursive = TRUE )
                               #return(dataset_name_csv)
                               # call performExperiment
                               #performExperiment(dataset_name_csv)
                             },
                             performExperiment = function(dataset_name) {
                               'Performs experiment. This is the main function, delegating responsibilities and monitoring the procedure.'
                               # (All graphs and files will be created from the classes responsible, which will have to call server$getDirectory)
                               # preprocess
                               # load dataset
                               dataset <- file_manipulator_$loadDataset(dataset_name)
                               # preprocess dataset
                               dictionary <- file_manipulator_$loadOrderedDictionary()
                               dataset <- cleaner_$convertAttributeTypes(dataset, dictionary)
                               #str(dataset)
                               algorithms <-  c(SvmClassifier$new(), AnnClassifier$new(), KnnClassifier$new(),
                                                BayesClassifier$new(), TreeClassifier$new())
                               
                               for(i in algorithms) {
                                 i
                                 task <- list(algorithm = class(i)[1])
                                 task$algorithm
                                 dataset <- expert_$choosePreprocessing(dataset, task)
                                 #str(dataset)
                                 # find metafeatures for parameter tuning 
                                 #metafeature_dataset <- mf2_extractor$get2MetaFeatures(dataset)
                                 # predict optimal hyperparameters
                                 opt_params <- optimizer_$optimizeHParam(metafeature_dataset, algorithm = task$algorithm)
                                 cat(opt_params)
                                 # train optimized model by calling each classifier's trainModel
                                 generic_classifier_$trainModel()
                                 
                                 
                               }
                               
                               # train optimal models
                               # build ensemble
                               # test final ensemble
                             },
                             trainScientist = function() {
                               
                             },
                             initialize=function(...) {
                               # store absolute paths for the moment(to be changed)
                               directories_ <<- setNames("/home/elena/R_ws/automl/automl/workspace",
                                                         c("Workspace"))
                               file_manipulator_ <<- FileManipulator$new(directories_ =directories_)
                               expert_ <<- Expert$new()
                               cleaner_ <<- Cleaner$new()
                               mf2_extractor <<- mf2Extractor$new()
                               optimizer_ <<- Optimizer$new()
                               generic_classifier_ <<- GenericClassifier$new()
                               callSuper(...)
                               .self
                             }
                           )
)
