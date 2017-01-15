
##' A class responsible for orchestrating the machine learning experiment.
##'
##' @import methods
##' @exportClass Server
##' @export Server
Server <- setRefClass(Class = "Server",
                           fields = list(
                             directories_ = "list",
                             cleaner_ = "Cleaner",
                             file_manipulator_ ="FileManipulator",
                             expert_ = "Expert",
                             optimizer_ ="Optimizer"
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
                               file_manipulator_$setDirectories(directories = directories_)
                               dataset <- file_manipulator_$loadDataset(dataset_name)
                               str(dataset)
                               # preprocess dataset
                               dictionary <- file_manipulator_$loadOrderedDictionary()
                               dataset <- cleaner_$convertAttributeTypes(dataset, dictionary)
                               #str(dataset)
                               algorithms <-  c(SvmClassifier$new(), AnnClassifier$new(),
                                                KnnClassifier$new(), BayesClassifier$new(),
                                                TreeClassifier$new())
                               task <- list()
                               for(classifier in algorithms) {
                                 algorithm <- list(algorithm = class(classifier)[1])
                                 task$algorithm <- algorithm
                                 preprocessed_dataset <- expert_$choosePreprocessing(dataset, task)
                                 str(preprocessed_dataset)
                                 # find metafeatures for parameter tuning 
                                 # there is the possibility of different metafeatures for each algorithm
                                 #metafeature_dataset <- mf2_extractor$get2MetaFeatures(dataset)
                                 # predict optimal hyperparameters
                                 opt_params <- optimizer_$optimizeHParam(metafeature_dataset, algorithm = algorithm)
                                 str(opt_params)
                                 # train optimized model by calling each classifier's trainModel
                                 model <- classifier$trainModel(training_dataset = preprocessed_dataset, parameters = opt_params, project_dir = directories_$Project)
                                 print(model)
                               }
                               
                               # train optimal models
                               # build ensemble
                               # test final ensemble
                             },
                             trainScientist = function() {
                               
                             },
                             initialize=function(...) {
                               # store absolute paths for the moment(to be changed)
                               directories_ <<- list()
                               file_manipulator_ <<- FileManipulator$new(directories_ = directories_)
                               expert_ <<- Expert$new()
                               cleaner_ <<- Cleaner$new()
                               mf2_extractor <<- mf2Extractor$new()
                               optimizer_ <<- Optimizer$new()
                               callSuper(...)
                               .self
                             }
                           )
)
