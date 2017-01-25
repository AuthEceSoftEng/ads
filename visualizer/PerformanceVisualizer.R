##' Α class responsible for generating grapchics describing performance of a model.
##'
##'
##' @import preprocessor 
##' @importClassesFrom preprocessor FeatureEngineer
##' @import methods
##' @exportClass FeatureVisualizer
##' @export FeatureVisualizer
PerformanceVisualizer <- setRefClass(Class = "PerformanceVisualizer",
                                 fields = list(

                                 ),
                                 methods = list(
                                   savePlots = function(...) {
                                     'Saves all plots produced during experiment and deemed informational'
                                     # choose informational plots
                                     # plot them
                                   },
                                   assessPlots = function(...) {
                                     'Assess if produced plots are informational'
                                   },
                                   initialize = function(...) {
                                     callSuper(...)
                                     .self
                                   }
                                 )
)
