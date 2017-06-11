##' Α class responsible for generating grapchics describing features of a dataset.
##'
##'
##' @import preprocessor 
##' @importClassesFrom preprocessor FeatureEngineer
##' @import methods
##' @exportClass FeatureVisualizer
##' @export FeatureVisualizer
FeatureVisualizer <- setRefClass(Class = "FeatureVisualizer",
                               fields = list(
                                 feature_engineer_ = "FeatureEngineer"
                                 

                               ),
                               methods = list(
                                visualizeFeatures = function(dataset, ...) {
                                  'Generates grapchics for categorical features.'
                                  # isolate categorical features
                                  factor_attributes <- names(dataset[sapply(dataset,class) == "factor"])
                                  #contains non-categorical variables (NOTE: non-character attributes could also be categorical, but how to recognize?)
                                  dataset_factor <- dataset[, (names(dataset) %in% factor_attributes)]
                                  # isolate numerical features
                                  dataset_numeric <- dataset[, !(names(dataset) %in% factor_attributes)]
                                  # isolate class
                                  dataset_class <- dataset$Class
                                  # call plot for each combination of class and attribute
                                  plots_a <- apply(dataset_factor, 2, plot.feature(), y = dataset_class )
                                  plots_b <- apply(dataset_numeric, 2, plot.feature(), y = dataset_class )
                                  # call plot between categorical attributes
                                  plots_c <- apply(dataset_factor, 2, function(u){
                                    apply(dataset_factor, 2, function(v){
                                      plot.feature(x = u, y = v)
                                    })
                                  })
                                  #out = matrix(unlist(bl), ncol=ncol(y), byerow=T)
                                  # call plot between numerical attributes
                                  plots_d <- apply(dataset_numeric, 2, function(x){
                                    apply(dataset_numeric, 2, function(y){
                                      plot.feature(x, y)
                                    })
                                  })
                                  # call plot between mixed
                                  plots_e <- apply(dataset_factor, 2, function(x){
                                    apply(dataset_numeric, 2, function(y){
                                      plot.feature(x, y)
                                    })
                                  })
                                  # call plot for each numeric attribute to find distribution
                                  plots_f <- apply(dataset_numeric, 2, plot.feature())
                                  # call plot for each categorical attribute
                                  plots_g <- apply(dataset_numeric, 2, plot.feature())
                                  # maybe call plot for the whole dataset
                                  plots_h <- plot.feature(x = dataset)
                                  # assess information of each graph
                                  plots <- c(plots_a, plots_b, plots_c, plots_d, plots_e, plots_f, plots_g, plots_h)
                                  assesGraphInformation(plots)
                                  # save important graphs
                                  # save KNOWLEDGE
                                },
                                visualizeCosmos = function(workspace_dir, meta_instance = NULL, ...) {
                                  file_manipulator <- FileManipulator$new(directories_= list(Workspace = workspace_dir))
                                  metafeatures     <- file_manipulator$loadRepoMetafeatures()
                                  dataset          <- metafeatures$dataset
                                  distance_scores  <- metafeatures$info$distance_scores
                                  means            <- metafeatures$info$means
                                  scales           <- metafeatures$info$scales
                                  pca_metafeatures <- prcomp(dataset, center = FALSE)
                                  contained_pca    <- pca_metafeatures$x[,1:3]
                                  x                <- contained_pca[,1]
                                  y                <- contained_pca[,2]
                                  z                <- distance_scores
                                  im               <- interp(x,y,z,duplicate = FALSE,linear = FALSE)
                                  with(im,image.plot(x,y,z, main = "Cosmos", xlab = "PCA1", ylab="PCA2"))
                                  image.plot( zlim=c(0,20), legend.only=TRUE)
                                  if(!is.null(meta_instance)) {
                                    meta_instance <- as.data.frame(meta_instance[, colnames(meta_instance) %in% colnames(metafeatures$dataset)])
                                    meta_instance <- as.data.frame(scale(meta_instance, center = means, scale = scales))
                                    total_dataset <- rbind(metafeatures$dataset, meta_instance)
                                    meta_instance_pca <- prcomp(total_dataset)
                                    x <- meta_instance_pca$x[nrow(total_dataset),1]
                                    y <- meta_instance_pca$x[nrow(total_dataset),2]
                                    points(x,y, col = "black", pch=19 )
                                  }
                                },
                                assessGraphInformation = function(plots, ...) {
                                  'Accepts a list of plots and returns the worth-printing ones.'
                                },
                                saveGraph = function(plot, ...) {
                                  'Saves a graph in default directory of current experiment.'
                                  # get directory from server
                                  # save
                                },
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


setGeneric(
  name = "plot.feature",
  def = function(x,y,z, ...) {
    standardGeneric("plot.feature")
  }
)

setMethod(
  f ="plot.feature",
  signature = c("numeric", "missing"),
  definition = function(x, y, ...) {
    # make histogram
    lower <- min(x)
    upper <- max(x)
    breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
    bwidth <- breaks[2]-breaks[1]
    histogram_plot <- ggplot(data=as.data.frame(x), aes(x)) +
      geom_histogram(binwidth=bwidth, fill = "white", colour = "black") + 
      labs(title = paste("Histogram for ", names(x)[1])) +
      labs(x = names(x)[1], y="Count") + 
      xlim(c(lower, upper)) +
      theme(plot.title = element_text(hjust = 0.5)) 
    # find distribution with hanging rootogram
    # makes sense only for count variables, so use preprocessor
    feature_engineer <- FeatureEngineer$new()
    x_counts <- feature_engineer$findCountFeatures(as.data.frame(x))
    root_pois <- rootogram(x, fitted = dpois, start = list(lambda = 1),
              breaks = lower:upper , plot = FALSE)
    root_nb <- rootogram(x, fitted = "negbin", breaks = lower:upper, plot = FALSE)
    root_hurdle <- NULL
    if( lower == 0) {
      cs_hnb <- hurdle(x ~ . , data = as.data.frame(x), dist = "negbin")
      root_hurdle <- rootogram(cs_hnb, max = 15, main = "Hurdle Negative Binomial")
    }
    
    ylims <- ylim(-2, 7)  # common scale for comparison
    if(is.null(root_hurdle)) {
      rootogram_plot <- cowplot::plot_grid(autoplot(root_pois) + ylims, autoplot(root_nb) + ylims, ncol =2, labels = "auto")
    }
    else {
      rootogram_plot <- cowplot::plot_grid(autoplot(root_pois) + ylims, autoplot(root_nb) + ylims,
                                           autoplot(root_hurdle) + ylims, ncol = 3, labels = "auto")
    }
    # box-plot
    box_plot <-  boxplot(x, horizontal = TRUE, xlab = names(x)[1], main = "Boxplot of numeric attribute")
    # estimated density
    dyes <- with(as.data.frame(x), density(x))
    density_plot <- plot(dyes, lty = 1, main = "Density function of numeric attribute")
    numeric_plots <- list(histogram_plot, rootogram_plot, box_plot, density_plot)
    return(numeric_plots)
  }
)

setMethod(
  f ="plot.feature",
  signature = c("factor", "missing"),
  definition = function(x, y, ...) {
    # make barplot
    barplot <- ggplot(as.data.frame(x), aes(x = x, fill = x)) + geom_bar()+
      scale_fill_hue(l=30)+
      xlab(names(x)[1]) +
      ylab("count") +
      ggtitle(paste("Barplot of attribute", names(x)[1] ))
    # make pie chart
    piechart <- plot_ly(data.frame(count(x)), labels = ~x, values = ~freq, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',
                 marker = list(colors = brewer.pal(3, "Accent")),
                 #The 'pull' attribute can also be used to create space between the sectors
                 showlegend = FALSE) %>%
      layout(title = paste('Pie chart of attribute ', names(x)[1]),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    # make hanging rootogram if ordered
    factor_plots <- list(barplot, piechart)
    return(factor_plots)
    
  }
)

setMethod(
  f ="plot.feature",
  signature = c("numeric", "numeric"),
  definition = function(x, y, ...){
    # correlation through scatter plot
    plot(x,y,xlim=c(min(x),max(x)), ylim=c(min(y),max(y)), 
         ylab = names(y)[1], xlab= names(x)[1], main="Scatterplot of numeric atrributes"
         )
    scatter_plot <- recordPlot()
    num_num_plots <- list(scatter_plot)
    return(num_num_plots)
  }
)

setMethod(
  f ="plot.feature",
  signature = c("factor", "factor"),
  definition = function(x, y, ...){
    # spineplot
    plot(y ~ x, data = data.frame(x,y))
    spine_plot <- recordPlot()
    # fourfold display
    vcd::fourfold(table(x,y))
    fourfold_plot <-recordPlot()
   
    fac_fac_plots <- list(spine_plot, fourfold_plot)
    return(fac_fac_plots)
  }
)


setMethod(
  f ="plot.feature",
  signature = c("numeric", "factor"),
  definition = function(x, y, ...){
    # this includes both graphs for class vs attr and numeric attr vs cat attr
    # they should certainly differ on descriptions, not necessarily different graphs
    # conditional density plot
    cd_plot <- graphics::cdplot(y ~ x, data = data.frame(x,y), main = "Conditional Density Plot",
                     xlab = names(x)[1], ylab = names(y)[2])
    # parallel boxplot
    plot(x ~y, data = data.frame(x, y),xlab = names(x)[1], ylab = names(y)[1])
    parallel_boxplot <- recordPlot()
    num_fac_plots <- list(cd_plot, parallel_boxplot)
    return(num_fac_plots)
  }
)

setMethod(
  f ="plot.feature",
  signature = c("numeric", "numeric", "factor"),
  definition = function(x, y, z, ...){
    # this includes both graphs for class vs attr and numeric attr vs cat attr
    # they should certainly differ on descriptions, not necessarily different graphs
    # conditional density plot
    lattice::xyplot(x~y|z, 
           main = paste("Scatterplots by ", names(z)[1]), 
           xlab= names(x)[1], ylab= names(y)[1])
    trellis_plot <- recordPlot()
    num_num_fac_plots <- list(trellis_plot)
    return(num_num_fac_plots)
  }
)

setMethod(
  f ="plot.feature",
  signature = c("data.frame"),
  definition = function(x,   ...){
    # barplot of means of each numeric attribute for each class level
    variables <- names(x[sapply(x,class) == "factor"])
    x_numeric <- x[, !(names(x) %in% variables)]
    x_numeric$Class <- x$Class
    melted <- reshape2::melt(x_numeric, ...)
    means.sem <- ddply(melted, c("Class", "variable"), summarise,
                       mean=mean(value, na.rm = TRUE), sem=sd(value)/sqrt(length(value)))
    means.sem <- transform(means.sem, lower=mean-sem, upper=mean+sem)
    
    means.barplot <- ggplot(means.sem, aes(x = Class, y = mean,  fill=variable)) +
      geom_bar(stat = "identity", position= "dodge") + 
      geom_errorbar(aes(ymin=lower, ymax=upper),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) +
      scale_colour_brewer(palette = "Accent")+
      xlab("") +
      ylab("") +
      ggtitle("")
    dataset_plots <- list(means.barplot)
    # mosaic of categorical attributes 
    x <- x[, (names(x) %in% variables)]
    columns <- ncol(x)
    for(i in seq(1, columns, by =3) ) {
      if( (i+1) > columns) {
        x_factor <- x[,c(i)]
      }
      else if ( (i+2) > columns) {
        x_factor <- x[,c(i, i+1)]
      }
      else {
        x_factor <- x[, c(i, i+1, i+2)]
      }
      x_factor$Class <- x$Class
      mosaicplot <- vcd::mosaic(~ . ,data = x_factor, shade=TRUE, legend=TRUE)
      dataset_plots[[ceiling(i/3)]] <- mosaicplot
    }
    return(dataset_plots)
    }
)
