##' Α class responsible for comparing the performance of different algorithms.
##'
##' Compared algorithms are either different machine learning models or optimization techniques. Their results depict their
##' performance on a problem. This class offers statistical hypothesis testing techniques (performance profilers, Friedman tests, Fisher's exact test,
##'  Chi-squared, Cochran-Mantel-Haenzel and Wilcoxon rank-sum tests)  to reject hypothesis of equality of algorithms and detect best-performing
##' algorithm (through Holm's step-down procedure). These are techniques for 1-way, 2-way and 3-way contigency tables.
##'
##'
##' @slot resutls_ result of applying hypothesis testing
##' @slot info_ list of information about HypothesisTester
##' 
##' @import methods
##' @import stats
##' @import tikzDevice
##' @import PMCMR
##' @import reshape2
##' 
##' @export 
HypothesisTester <- setRefClass(Class = "HypothesisTester",
                                    fields = list(
                                      results_ = "list",
                                      info_    = "list"
                                    )
                                )
HypothesisTester$methods(
  #' Plot a performance profile
  #' 
  #' A plot visualizing the comparison of performances between different algorithms on different problems is provided.
  #' For more information on performance profile plots please refer to \url{https://arxiv.org/pdf/cs/0102001.pdf}.
  #' 
  #' @name performanceProfiler
  #' @alias performanceProfiler
  #' 
  #' @param performances a data.frame with columns corresponding to different algorithms and rows to different datasets
  #' @param file_name name of file to save plot
  #' @param directory of current project
  #' 
  #' @return TRUE upon successful plot generation
  performanceProfiler = function(performances, file_name, project_dir, ...) {
    'Creates performance profiles of different algorithms. Performances is a data.frame with columns 
    corresponding to different algorithms and rows to different datasets'
    performances$X <- NULL
    perform_profiles <- data.frame()
    # find minimum of each column
    alg_min <- apply(performances, 1, function(x) min(x, na.rm = TRUE))
    # compute ratio of each element by dividing with smallest dataset performance
    perform_profiles <- performances/alg_min
    # replace all NaN's with twice the max ratio (a Nan represents an unsolved dataset)
    max_ratio <- max(perform_profiles)
    perform_profiles[is.na(perform_profiles)] <- 2*max_ratio
    # sort each column in ascengind order
    perform_profiles <- as.data.frame(apply(perform_profiles, 2, sort))
    # -------- plot stair graph (one line per algorithm, showing what percentage of algorithms performs better on each dataset)--------
    # find points to plot
    perform_profiles$x <- (seq(1, nrow(perform_profiles)))/nrow(perform_profiles)
    # plot
    perform_profiles <- melt(perform_profiles ,  id.vars = 'x', variable.name = 'algorithm')
    result = tryCatch({
      tikz(paste(file.path(project_dir, file_name), ".tex", sep = ""), standAlone = TRUE, width=5, height=5)
      ggplot(perform_profiles, aes(value, x)) + geom_line(aes(colour = algorithm)) +
        ggtitle('Performance profile plot') + 
        labs(x="t",y="P")
      dev.off()
      
      TRUE
    }, warning = function(w) {
      FALSE
    }, error = function(e) {
      FALSE
    })
    ggplot(perform_profiles, aes(value, x)) + geom_line(aes(colour = algorithm)) +
      ggtitle('Performance profile plot') + 
      labs(x="t",y="P")
    return(result)
  },
  #' Test for ranking of algorithms
  #' 
  #' Offers statistical hypothesis tests for comparing the performance of different algorithms on different datasets.
  #' One can choose to apply more than one tests but personal care must be taken in order that \code{"results"} has the same
  #' form for all required tests.
  #' 
  #' @name rankTest
  #' @alias rankTest
  #' 
  #' @param results contains performances of different algorithms on different problems
  #' @param methods desired statistical tests. Available tests include chi-square, friedman, fisher,
  #'  wilcoxon, cochran-mante-haenszel, mcneamar.
  #' @param conf.level desired level of confidence of hypothesis test.
  #' 
  #' @return NULL
  rankTest = function(results, methods = c( "chi-squared", "friedman",
                                          "fisher", "wilcoxon", "cochran-mante-haenszel",
                                         "mcnemar"), conf.level, ... ) {
   'Returns tests included in methods. Available tests include
    hypothesis testing techniques on 1-way, 2-way and 3-way contigency tables'
    test_results <- list()
    # 1-way
    if("chi-squared" %in% methods) {
      # x is number of successes and n is number of trials 
      chi_test <- prop.test(x, n,  conf.level = conf.level)
      test_results[["chi-squared"]] <- chi_test
    }
    # 2-way
    if("friedman" %in% methods) {
      friedman_test <-friedman.test(as.matrix(results), conf.level = conf.level)
      test_results[["friedman"]] <- friedman_test
    }
    if("fisher" %in% methods) {
      # x and y are row vectors with same length
      fisher_test <- fisher.test(results,  conf.level = conf.level)
      test_results[["fisher"]] <- fisher_test
    }
    if("wilcoxon" %in% methods) {
      # same with man-whitney u test
      # x and y are row vectors with same length
      x <- unlist(results[[1]])
      y <- unlist(results[[2]])
      wilcoxon_test <- stats::wilcox.test(x, y,  conf.level = conf.level)
      test_results[["wilcoxon"]] <- wilcoxon_test
    }
    if("cochran-mante-haenszel" %in% methods) {
      # x is a 3-way contigency array  
      mantelhaen_list <- stats::mantelhaen.test(x,   conf.level = conf.level, correct = FALSE)
      test_results[["cochran-mante-haenszel"]] <- wilcoxon_test
    }
    if("mcnemar" %in% methods) {
      # x is a 3-way contigency array  
      mcnemar_list <- stats::mcnemar.test(x,  conf.level = conf.level, correct = FALSE)
      test_results[["mcnemar"]] <- mcnemar_test
    }
    info_$methods <<- methods
    return(test_results)
  },
  #' Alternative Friedman test
  #' 
  #' Offers an alternative Friedman test differing from the original at the statistic. As described in \url{http://dl.acm.org/citation.cfm?id=1248547.1248548} the 
  #' original statistic is undesirably conservative. 
  #' 
  #' @name alternativeFriedman
  #' @alias alternativeFriedman
  #' 
  #' @param original_friedman an object of class "htest" produced by appying \code{\link[stats]{friedman.test}} 
  #' @param N number of datasets
  #' @param k number of algorithms
  #' @param thres threshold
  #' @param alpha_level confidence level
  #' 
  #' @return the altered friedman result, an object of class "htest"
  alternativeFriedman = function(original_friedman, N, k, thres = 0.0001, alpha_level = 0.05) {
    'Replace friedman chi-squared statistic with f-statistic' 
    Fr                      <- original_friedman$statistic
    f_alt                   <- ((N-1)*Fr)/(N*(k-1)-Fr)
    friedman_test           <- list()
    friedman_test$statistic <- f_alt
    p-value                 <- pf(1-alpha_level, df1 = k-1, df2 = (k-1)*(N-1))
    diff                    <- (p-value - alpha_level)
    if(abs(diff) < -thres) {
      friedman_test$rejected <- TRUE
    } else {
      friedman_test$rejected <- FALSE
    } 
    return(friedman_test)
  },
  #' Friedman test with post-hoc
  #' 
  #' Offers an alternative Friedman test differing from the original at the statistic. As described in \url{http://dl.acm.org/citation.cfm?id=1248547.1248548} the 
  #' original statistic is undesirably conservative. 
  #' 
  #' @name friedman.test.with.post.hoc
  #' @alias friedman.test.with.post.hoc
  #' 
  #' @param formu
  #' @param data
  #' @param to.print.friedman
  #' @param to.post.hoc.if.signif 
  #' @param to.plot.parallel
  #' @param to.plot.boxplot = T
  #' @param signif.P = .05
  #' @param color.blocks.in.cor.plot
  #' @param jitter.Y.in.cor.plot
  #' 
  #' @return result
  friedman.test.with.post.hoc = function(formu, data, to.print.friedman = T, to.post.hoc.if.signif = T,  to.plot.parallel = T,
                                         to.plot.boxplot = T, signif.P = .05, color.blocks.in.cor.plot = T, jitter.Y.in.cor.plot =F)
  {
    'Function taken from https://www.r-statistics.com/tag/friedman-test/'
    # formu is a formula of the shape: 	Y ~ X | block
    # data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor) ]]
    # Note: This function doesn't handle NA's! In case of NA in Y in one of the blocks, then that entire block should be removed.
    # get the names out of the formula
    formu.names <- all.vars(formu)
    Y.name <- formu.names[1]
    X.name <- formu.names[2]
    block.name <- formu.names[3]
    if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]	# In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...
    
    # Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.
    
    # stopping in case there is NA in the Y vector
    if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")
    
    # make sure that the number of factors goes with the actual values present in the data:
    data[,X.name ] <- factor(data[,X.name ])
    data[,block.name ] <- factor(data[,block.name ])
    number.of.X.levels <- length(levels(data[,X.name ]))
    if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}
    
    # making the object that will hold the friedman test and the other.
    the.sym.test <- symmetry_test(formu, data = data,	### all pairwise comparisons
                                  teststat = "max",
                                  xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
                                  ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
    )
    # if(to.print.friedman) { print(the.sym.test) }
    if(to.post.hoc.if.signif)
    {
      if(pvalue(the.sym.test) < signif.P)
      {
        # the post hoc test
        The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")	# this is the post hoc of the friedman test
        # plotting
        if(to.plot.parallel & to.plot.boxplot)	par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
        
        if(to.plot.parallel)
        {
          X.names <- levels(data[, X.name])
          X.for.plot <- seq_along(X.names)
          plot.xlim <- c(.7 , length(X.for.plot)+.3)	# adding some spacing from both sides of the plot
          
          if(color.blocks.in.cor.plot)
          {
            blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
          } else {
            blocks.col <- 1 # black
          }
          data2 <- data
          if(jitter.Y.in.cor.plot) {
            data2[,Y.name] <- jitter(data2[,Y.name])
            par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"
          } else {
            par.cor.plot.text <- "Parallel coordinates plot"
          }
          # adding a Parallel coordinates plot
          matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
                                    direction="wide")[,-1])  ,
                  type = "l",  lty = 1, axes = FALSE, ylab = Y.name,
                  xlim = plot.xlim,
                  col = blocks.col,
                  main = par.cor.plot.text)
          axis(1, at = X.for.plot , labels = X.names) # plot X axis
          axis(2) # plot Y axis
          points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
        }
        if(to.plot.boxplot)
        {
          # first we create a function to create a new Y, by substracting different combinations of X levels from each other.
          subtract.a.from.b <- function(a.b , the.data)
          {
            the.data[,a.b[2]] - the.data[,a.b[1]]
          }
          temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
                               direction="wide") 	#[,-1]
          wide.data <- as.matrix(t(temp.wide[,-1]))
          colnames(wide.data) <- temp.wide[,1]
          Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
          names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
          the.ylim <- range(Y.b.minus.a.combos)
          the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))	# adding some space for the labels
          is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
          boxplot(Y.b.minus.a.combos,
                  names = names.b.minus.a.combos ,
                  col = is.signif.color,
                  main = "Boxplots (of the differences)",
                  ylim = the.ylim
          )
          legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
          abline(h = 0, col = "blue")
        }
        list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
        if(to.print.friedman) {print(list.to.return)}
        return(list.to.return)
      }	else {
        print("The results where not significant, There is no need for a post hoc test")
        return(the.sym.test)
      }
    }
  },     
  #' Return information about HypothesisTester
  #' 
  #' Information includes methods used for statistical testing.
  #' 
  #' @name getInfo
  #' @alias getInfo
  #' 
  #' 
  #' @return list of information
  getInfo = function(...) {
    'Return information about hypothesis testing'
    return(info_)
  },
  initialize = function(...) {
    results_ <<- list()
    info_ <<- list()
    callSuper(...)
    .self
  }                                      
)
                                    

