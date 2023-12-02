#'@include 0-utility-functions.R 1-log-functions.R
NULL

# Generic ----------------------------------------------------------------------

#'Stat Function Doc
#'
#'
#'@param x \code{matrix} or \code{data.frame}.
#'@param g a vector or factor object giving the group for the corresponding
#'elements of \code{x}.
#'@param y numeric vector of data values. Must have the same length as \code{ncol(x)}.
#'
#'@return A list containing two elements:
#'
#'\describe{
#'   \item{statistic}{A numeric vector, the values of the test statistic}
#'   \item{significance}{A numeric vector, the p-values of the selected test}
#'}
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
#'
#'@name genericRowBioStat
NULL

#'Create a design matrix
#'
#'@description Simple function to generate a design matrix when the input is a
#'vector or a factor. If the input is a matrix, it is returned as is.
#'See the **Details** section below for further information.
#'
#'@param y a \code{vector}, \code{factor} or \code{matrix}
#'
#'@return A \code{matrix}.
#'
#'@details This function is internally used to create a design matrix when not
#'directly provided.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'\dontrun{
#'#Categorical vector
#'y = c(rep("a",5), rep("b",5))
#'names(y) = paste0("s",seq(10))
#'createDesignMatrix(y)
#'
#'#Factor
#'y = as.factor(y)
#'createDesignMatrix(y)
#'
#'#Numerical vector with 2 categories
#'y = c(rep(1,5), rep(0,5))
#'names(y) = paste0("s",seq(10))
#'createDesignMatrix(y)
#'
#'#Numerical vector
#'y = sample(x = seq(from=20,to=80),10)
#'names(y) = paste0("s",seq(10))
#'createDesignMatrix(y)
#'
#'#Design matrix
#'y = matrix(
#'  data = c(
#'     1,1,1,0,0,0,
#'     0,0,0,1,1,1
#'  ),
#'  nrow = 6,
#'  ncol = 2,
#'  dimnames = list(
#'    paste0("S",seq(6)),
#'    c("wt", "mut")
#'  )
#')
#'createDesignMatrix(y)
#'
#'#Numerical matrix
#'y = matrix(
#'  data = c(
#'     sample(x = seq(from=20,to=80),6),
#'     0,0,0,1,1,1
#'  ),
#'  nrow = 6,
#'  ncol = 2,
#'  dimnames = list(
#'    paste0("S",seq(6)),
#'    c("age", "p53mut")
#'  )
#')
#'createDesignMatrix(y)
#'}
#'@keywords internal
createDesignMatrix <- function(y){
  #default
  out = NULL

  #check if y is a vector
  if(is.null(dim(y))){

    #Set the design matrix
    out = stats::model.matrix(object = ~0 + y);

    if(!is.null(names(y))){rownames(out) = names(y)}

    if(isTRUE(ncol(out)>1)){
      #check colnames
      cnames = make.names(
        names = substring(text = colnames(out), first = 2),
        unique = T,
        allow_ = T
      )

      #clean colnames (removes the 'y' from names)
      colnames(out) = cnames
    }

  } else {
    #If y is a matrix, use as design matrix
    out = y;
  }

  return(out)
}


#'Create custom contrasts
#'
#'@description Simple function to generate default contrasts.
#'See the **Details** section below for further information.
#'
#'@param x a factor, a character vector or an object with column names providing
#'the names of the parameters of which contrasts are desired
#'@param ref character string indicating the reference class
#'@param target character string indicating the target class
#'
#'@return A character string or a vector of strings representing contrasts
#'between a set of parameters.
#'
#'@details This function is internally used to create contrasts when not
#'directly provided in the specific cases of binomial and multinomial response.
#'
#'There are four main settings.
#'
#'If \code{target} and \code{ref} are provided, the contrast is created so that
#'the target class is compared against the reference (i.e., target - ref).
#'
#'If \code{target} is provided and \code{ref = NULL}, the contrast is created so
#'that the target class is compared against the other classes (i.e., target -
#'(class1 + class2 + ...)).
#'
#'If \code{ref} is provided and \code{target = NULL}, the contrasts are created so
#'that the each class is compared against the reference classes (e.g., class1 -
#'ref).
#'
#'If \code{target} and \code{ref} are \code{NULL}, the contrasts are created so
#'that each class is compared against the remaining classes (e.g., class1 -
#'(class2 + class3), class2 - (class1 + class3), class3 - (class1 + class2)).
#'
#'If \code{target = NULL} and the parameter names from \code{x} are two, then
#'the first parameter is considered as the target class.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'\dontrun{
#'#Character vector (binomial)
#'x = c("a", "b")
#'createContrasts(x)
#'createContrasts(x, "a")
#'createContrasts(x, NULL, "b")
#'createContrasts(x, "a", "b")
#'
#'#Character vector
#'x = c("a", "b", "c")
#'createContrasts(x)
#'createContrasts(x, "a")
#'createContrasts(x, NULL, "b")
#'createContrasts(x, "a", "b")
#'
#'#Factor
#'x = as.factor(sample(c("a", "b", "c"),10,replace=T))
#'createContrasts(x)
#'createContrasts(x, "a")
#'createContrasts(x, NULL, "b")
#'createContrasts(x, "a", "b")
#'}
#'@keywords internal
createContrasts <- function(
    x,
    target = NULL,
    ref = NULL
  ){
  #setup
  out = NULL

  #check input
  if(is.factor(x)){
    lvls = levels(x)
  } else if(isTRUE(is.character(x))){
    lvls = x
  } else if(!is.null(dim(x))){
    lvls = colnames(x)
  } else {
    stop("'x' must be a factor, a character vector or an object with the parameter names as column names")
  }

  if(!is.null(ref)){
    if (length(ref) != 1L)
      stop("'ref' must be of length one")
    hasRef <- isFALSE(is.na(match(ref, lvls)))
    if(!hasRef)
      stop("'ref' must be an existing level")
  } else {
    hasRef = FALSE
  }
  if(!is.null(target)){
    if (length(target) != 1L)
      stop("'target' must be of length one")
    hasTarget <- isFALSE(is.na(match(target, lvls)))
    if(!hasTarget)
      stop("'target' must be an existing level")
  } else {
    if (length(lvls) == 2L){
      target = lvls[1]
      hasTarget = TRUE
    } else {
      hasTarget = FALSE
    }
  }

  if(isTRUE(hasTarget && hasRef)){
    out = paste(target, ref, sep = "-")
  } else if(isTRUE(hasTarget && !hasRef)){
    refs = setdiff(lvls, target)
    out = paste(refs, collapse = "+")
    if(length(refs)>1){
      out = paste0("(",out,")")
    }
    out = paste(target, out, sep = "-")
  } else if(isTRUE(!hasTarget && hasRef)){
    out = paste(setdiff(lvls, ref), ref, sep = "-")
  } else if(isTRUE(!hasTarget && !hasRef)){
    out = sapply(X = lvls, FUN = createContrasts, x = x, ref = NULL, USE.NAMES = FALSE)
  }

  return(out)
}


#'Create a matrix of custom contrasts
#'
#'@description Simple function to generate a default contrast matrix from a
#'design matrix or factor.
#'See the **Details** section below for further information.
#'
#'@inheritParams createContrasts
#'
#'@return A numeric matrix representing contrasts between a set of parameters.
#'
#'@details This function is internally used to create a contrast matrix when not
#'directly provided in the specific cases of binomial and multinomial response.
#'
#'There are four main settings.
#'
#'If \code{target} and \code{ref} are provided, the contrast is created so that
#'the target class is compared against the reference (i.e., target - ref).
#'
#'If \code{target} is provided and \code{ref = NULL}, the contrast is created so
#'that the target class is compared against the other classes (i.e., target -
#'(class1 + class2 + ...)).
#'
#'If \code{ref} is provided and \code{target = NULL}, the contrasts are created so
#'that the each class is compared against the reference classes (e.g., class1 -
#'ref).
#'
#'If \code{target} and \code{ref} are \code{NULL}, the contrasts are created so
#'that each class is compared against the remaining classes (e.g., class1 -
#'(class2 + class3), class2 - (class1 + class3), class3 - (class1 + class2)).
#'
#'@inherit createContrasts author
#'
#'@examples
#'\dontrun{
#'#Character vector (binomial)
#'x = c("a", "b")
#'createContrastMatrix(x)
#'createContrastMatrix(x, "a")
#'createContrastMatrix(x, NULL, "b")
#'createContrastMatrix(x, "a", "b")
#'
#'#Character vector
#'x = c("a", "b", "c")
#'createContrastMatrix(x)
#'createContrastMatrix(x, "a")
#'createContrastMatrix(x, NULL, "b")
#'createContrastMatrix(x, "a", "b")
#'
#'#Factor
#'x = as.factor(sample(c("a", "b", "c"),10,replace=T))
#'createContrastMatrix(x)
#'createContrastMatrix(x, "a")
#'createContrastMatrix(x, NULL, "b")
#'createContrastMatrix(x, "a", "b")
#'}
#'@keywords internal
createContrastMatrix <- function(
    x,
    target = NULL,
    ref = NULL
){
  #setup
  out = NULL
  #contrasts
  out = createContrasts(x=x,target=target,ref=ref)
  #contrast matrix
  out = limma::makeContrasts(contrasts = out)
  return(out)
}


# Empirical Bayes Statistics ---------------------------------------------------

#'Empirical Bayes Moderated Statistics
#'
#'@description This function implements a common workflow to fit linear models
#'and compute moderated t-statistics and moderated F-statistic by empirical Bayes
#'moderation of the standard errors towards a global value.
#'
#'If the input data was generated with microarray technology, the following
#'steps are executed:
#'
#'\enumerate{
#' \item linear model fitting for each feature: \code{\link[limma]{lmFit}}
#' \item compute coefficients and standard errors for a given set of contrasts: \code{\link[limma]{contrasts.fit}}
#' \item compute moderated t-statistics and moderated F-statistic: \code{\link[limma]{eBayes}}
#' \item summarise the linear model fit: \code{\link[limma]{topTable}}
#'}
#'
#'If the input data was generated with RNA-seq technology, the following
#'steps are executed:
#'
#'\enumerate{
#' \item (optional) transform count data for linear modelling: \code{\link[limma]{voom}}
#' \item linear model fitting for each feature: \code{\link[limma]{lmFit}}
#' \item compute coefficients and standard errors for a given set of contrasts: \code{\link[limma]{contrasts.fit}}
#' \item compute moderated t-statistics and moderated F-statistic: \code{\link[limma]{eBayes}}
#' \item summarise the linear model fit: \code{\link[limma]{topTable}}
#'}
#'
#'For complete details of each step see the manual pages of the respective
#'functions.
#'
#'See the **Details** section below for further information.
#'
#'@param x a matrix-like data object with rows corresponding to genes and
#'columns to observations.
#'@param y a vector, factor or matrix. It is used to create a design matrix if
#'not explicitly provided via the \code{design} argument.
#'@param observations (optional) integer vector, the indices of observations to
#'keep.
#'@param technology character string, the technology used to generate the data.
#'Available options are:
#'\describe{
#'   \item{array}{data generated with microarray technology}
#'   \item{seq}{data generated with RNA-seq technology}
#'}
#'@param statistic character string, indicating the moderated statistic to return.
#'@param is.logged logical, whether the original data is already logged. If
#'\code{is.logged = FALSE}, the data is internally transformed.
#'@param mean.variance character string indicating whether the mean-variance
#'relationship should be modeled with precision weights (\code{mean.variance =
#'"weights"}) or with an empirical Bayes prior trend (\code{mean.variance =
#'"ebayes"}).
#'@param logger a \code{\link{Logger}} object.
#'@param ... further arguments to \code{\link[limma]{lmFit}}.
#'
#'@inheritParams limma::lmFit
#'@inheritParams limma::contrasts.fit
#'@inheritParams limma::eBayes
#'@inheritParams limma::topTable
#'@inheritParams limma::voom
#'
#'@return A list containing two elements:
#'
#'\describe{
#'   \item{statistic}{A numeric vector, the values of the test statistic}
#'   \item{significance}{A numeric vector, the p-values of the selected test}
#'}
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define row/col size
#'nr = 20
#'nc = 20
#'
#'#Data
#'x = matrix(
#'  data = stats::rnorm(n = nr*nc),
#'  nrow = nr,
#'  ncol = nc,
#'  dimnames = list(
#'    paste0("g",seq(nr)),
#'    paste0("S",seq(nc))
#'  )
#')
#'
#'#Categorical output vector (binomial)
#'y = c(rep("wt",nc/2), rep("mut",nc/2))
#'names(y) = paste0("S",seq(nc))
#'rowEBayesStatistics(x=x,y=y)
#'
#'
#'#Categorical output vector (multinomial)
#'y = sample(x = c("I","II","III"),size=nc,replace=TRUE)
#'names(y) = paste0("S",seq(nc))
#'rowEBayesStatistics(x=x,y=y)
#'
#'#Numerical output vector
#'y = sample(x = seq(from=20,to=80),size=nc,replace=TRUE)
#'names(y) = paste0("S",seq(nc))
#'rowEBayesStatistics(x=x,y=y,statistic='moderated.t')
#'
#'#Design matrix
#'y = matrix(
#'  data = c(
#'     c(rep(1,nc/2), rep(0,nc/2)),
#'     c(rep(0,nc/2), rep(1,nc/2))
#'  ),
#'  nrow = nc,
#'  ncol = 2,
#'  dimnames = list(
#'    paste0("S",seq(nc)),
#'    c("wt", "mut")
#'  )
#')
#'rowEBayesStatistics(x=x,y=y)
#'
#'#Numerical matrix
#'y = matrix(
#'  data = c(
#'     sample(x = seq(from=20,to=80),size=nc,replace=TRUE),
#'     sample(x = c(0,1),size=nc,replace=TRUE)
#'  ),
#'  nrow = nc,
#'  ncol = 2,
#'  dimnames = list(
#'    paste0("S",seq(nc)),
#'    c("age", "p53mut")
#'  )
#')
#'rowEBayesStatistics(x=x,y=y)
#'
#'@export
rowEBayesStatistics <- function(
    x,
    y = NULL,
    observations = NULL,

    #further arguments
    statistic  = c("moderated.F", "moderated.t"),
    technology = c("array","seq"),
    is.logged  = TRUE,
    mean.variance = c("ebayes", "weights"),

    #voom
    span = 0.5,

    #lmFit
    method      = c("ls", "robust"),
    design      = NULL,
    weights     = NULL,
    ndups       = NULL,
    spacing     = NULL,
    block       = NULL,
    correlation = NULL,

    #contrasts.fit
    contrasts = NULL,

    #eBayes
    proportion     = 0.01,
    stdev.coef.lim = c(0.1, 4),
    robust         = FALSE,
    winsor.tail.p  = c(0.05, 0.1),

    #topTable, decideTests,topTableF
    coef          = NULL,
    adjust.method = "BH",

    #logger
    logger = NULL,
    ...
  ){

  # Setup ---------------------------------------------------------------------#
  technology = match.arg(technology)
  method = match.arg(method)
  mean.variance = match.arg(mean.variance)
  statistic = match.arg(statistic)

  if(isTRUE(is.null(y) && is.null(design))){stop("'design' and/or 'y' must be provided.")}

  logger = openCon(logger)
  rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");

  # Subset
  if(isTRUE(!is.null(observations))){
    x      =      x[,observations,drop=F]
    design = design[observations,,drop=F]
    block  = block[observations]
    if(isTRUE(!is.null(weights))){
      if(isTRUE(is.null(dim(weights)))){
        weights = weights[observations]
      } else {
        if(isTRUE(ncol(x)==ncol(weights))){
          weights = weights[,observations,drop=F]
        } else {stop("'ncol(weights)' different from 'ncol(x)'. Please, check your input.\n")}
      }
    }
  }

  #check design matrix
  if(!is.null(design)){
    if(!is.null(y)){
      logTrace(object = logger, message = "Check provided design matrix.", sep = "\n", add.level = TRUE, add.time = TRUE)
      #check in common
      inter = intersect(x = colnames(x), y = rownames(design))
      #update
      if(length(inter)!=ncol(x)){
        warning("There is a problem with the provided design matrix. 'y' will be used as design matrix.\n")
        design = y;
      } else {
        design = design[inter,,drop=F]
      }
    }
  } else {
    design = createDesignMatrix(y)
  }

  #check if any empty column
  is.zero = which(colSums(x = (design != 0), na.rm = TRUE) == 0)
  #remove
  if(length(is.zero)>0){
    design = design[,-is.zero,drop=F]
  }

  # Pipeline ------------------------------------------------------------------#
  #Design matrix should be full rank
  if(isTRUE(limma::is.fullrank(design))){

    #Limma expects log2 data, so log the data if not already log2
    if(isFALSE(is.logged)){
      logTrace(object = logger, message = "Apply logarithmic transformation to data.", sep = "\n", add.level = TRUE, add.time = TRUE)
      x = log2(x);
    }

    #RNA-seq: Model the mean-variance relationship with precision weights
    if(identical(technology, "seq") && identical(mean.variance, "weights")){
      logTrace(object = logger, message = "Model the mean-variance relationship with precision weights.", sep = "\n", add.level = TRUE, add.time = TRUE)
      x = limma::voom(
        counts      = x,
        design      = design,
        block       = block,
        weights     = weights,
        correlation = correlation,
        span        = span,
        plot        = FALSE,
        save.plot   = FALSE
      )
    }

    #Fit the model (x has rows corresponding to genes and columns to samples)
    logDebug(object = logger, message = "Fitting a linear model for each feature given a series of observations...", sep = "", add.level = TRUE, add.time = TRUE)
    limmaFit <- limma::lmFit(
      object      = x,
      design      = design,
      method      = method,
      weights     = weights,
      ndups       = ndups,
      spacing     = spacing,
      block       = block,
      correlation = correlation
      ,...
    )
    logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)


    if(!is.null(contrasts)){
      #Compute the coefficients and the standard errors for the given set of contrasts
      logDebug(object = logger, message = "Computing estimated coefficients and standard errors for a given set of contrasts...", sep = "", add.level = TRUE, add.time = TRUE)
      limmaFit <- limma::contrasts.fit(
        fit = limmaFit,
        contrasts = contrasts
      )
      logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    }

    #Compute statistics on the model by empirical Bayes moderation of the standard errors towards a common value
    logDebug(object = logger, message = "Computing moderated t-/F-statistic, and log-odds of differential expression by empirical Bayes moderation of the standard errors towards a common value...", sep = "", add.level = TRUE, add.time = TRUE)
    trend = if(identical(technology, "seq") && identical(mean.variance, "ebayes")){TRUE}else{FALSE}
    limmaEBayes <- limma::eBayes(
      fit            = limmaFit,
      proportion     = proportion,
      stdev.coef.lim = stdev.coef.lim,
      robust         = robust,
      winsor.tail.p  = winsor.tail.p,
      trend          = trend
    )
    logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)


    #Get a list of top-ranked genes differential expressed from a previous linear model fit.
    #We adjust the p-value for multiple testing.
    logDebug(object = logger, message = "Extracting a table of the top-ranked genes from the linear model fit...", sep = "", add.level = TRUE, add.time = TRUE)
    limmaTop = limma::topTable(
      fit           = limmaEBayes,
      coef          = coef,
      number        = Inf,
      genelist      = limmaEBayes$genes,
      adjust.method = adjust.method,
      p.value       = 1,
      sort.by       = "none",
      fc            = NULL,
      lfc           = NULL,
      confint       = FALSE
    )
    logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #Extract values
    out = switch(
      statistic,
      "moderated.t" = limmaTop$t,
      "moderated.F" = limmaTop$F
    )

    if(isTRUE(is.null(out))){
      stop(paste(
        statistic,
        "statistic was not found in the dataframe returned by limma::topTable.",
        "Check 'x', 'design', 'coef', and 'contrasts' parameters."
      ))
    }

    #output
    out = list(statistic = out, significance = limmaTop$P.Value)

  } else {
    logTrace(object = logger, message = "Design matrix is not full rank.", sep = "\n", add.level = TRUE, add.time = TRUE)
    out = list(statistic = NULL, significance = NULL)
  }


  # End -----------------------------------------------------------------------#
  closeCon(logger)
  return(out)
}

#'Empirical Bayes Moderated F-Statistic
#'
#'@description This function implements a common workflow to fit linear models
#'and compute moderated moderated F-statistic by empirical Bayes
#'moderation of the standard errors towards a global value.
#'
#'If the input data was generated with microarray technology, the following
#'steps are executed:
#'
#'\enumerate{
#' \item linear model fitting for each feature: \code{\link[limma]{lmFit}}
#' \item compute coefficients and standard errors for a given set of contrasts: \code{\link[limma]{contrasts.fit}}
#' \item compute moderated moderated F-statistic: \code{\link[limma]{eBayes}}
#' \item summarise the linear model fit: \code{\link[limma]{topTable}}
#'}
#'
#'If the input data was generated with RNA-seq technology, the following
#'steps are executed:
#'
#'\enumerate{
#' \item (optional) transform count data for linear modelling: \code{\link[limma]{voom}}
#' \item linear model fitting for each feature: \code{\link[limma]{lmFit}}
#' \item compute coefficients and standard errors for a given set of contrasts: \code{\link[limma]{contrasts.fit}}
#' \item compute moderated moderated F-statistic: \code{\link[limma]{eBayes}}
#' \item summarise the linear model fit: \code{\link[limma]{topTable}}
#'}
#'
#'For complete details of each step see the manual pages of the respective
#'functions.
#'
#'@inheritParams rowEBayesStatistics
#'
#'@inherit rowEBayesStatistics return
#'
#'@inherit rowEBayesStatistics author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define row/col size
#'nr = 20
#'nc = 20
#'
#'#Data
#'x = matrix(
#'  data = stats::rnorm(n = nr*nc),
#'  nrow = nr,
#'  ncol = nc,
#'  dimnames = list(
#'    paste0("g",seq(nr)),
#'    paste0("S",seq(nc))
#'  )
#')
#'
#'
#'#Categorical output vector (multinomial)
#'y = sample(x = c("I","II","III"),size=nc,replace=TRUE)
#'names(y) = paste0("S",seq(nc))
#'rowEBayesStatistics(x=x,y=y)
#'
#'@export
rowModeratedOneWayAnova <- function(
    x,
    y = NULL,
    observations = NULL,

    #further arguments
    technology = c("array","seq"),
    is.logged  = TRUE,
    mean.variance = c("ebayes", "weights"),

    #voom
    span = 0.5,

    #lmFit
    method      = c("ls", "robust"),
    design      = NULL,
    weights     = NULL,
    ndups       = NULL,
    spacing     = NULL,
    block       = NULL,
    correlation = NULL,

    #contrasts.fit
    contrasts = NULL,

    #eBayes
    proportion     = 0.01,
    stdev.coef.lim = c(0.1, 4),
    robust         = FALSE,
    winsor.tail.p  = c(0.05, 0.1),

    #topTable
    coef          = NULL,
    adjust.method = "BH",

    #logger
    logger = NULL,
    ...
){

  # Setup ---------------------------------------------------------------------#
  technology = match.arg(technology)
  method = match.arg(method)
  mean.variance = match.arg(mean.variance)
  statistic = match.arg(statistic)

  if(isTRUE(is.null(y) && is.null(design))){stop("'design' and/or 'y' must be provided.")}

  logger = openCon(logger)
  rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");

  # Subset
  if(isTRUE(!is.null(observations))){
    x      =      x[,observations,drop=F]
    design = design[observations,,drop=F]
    block  = block[observations]
    if(isTRUE(!is.null(weights))){
      if(isTRUE(is.null(dim(weights)))){
        weights = weights[observations]
      } else {
        if(isTRUE(ncol(x)==ncol(weights))){
          weights = weights[,observations,drop=F]
        } else {stop("'ncol(weights)' different from 'ncol(x)'. Please, check your input.\n")}
      }
    }
  }

  #check design matrix
  if(!is.null(design)){
    if(!is.null(y)){
      logTrace(object = logger, message = "Check provided design matrix.", sep = "\n", add.level = TRUE, add.time = TRUE)
      #check in common
      inter = intersect(x = colnames(x), y = rownames(design))
      #update
      if(length(inter)!=ncol(x)){
        warning("There is a problem with the provided design matrix. 'y' will be used as design matrix.\n")
        design = y;
      } else {
        design = design[inter,,drop=F]
      }
    }
  } else {
    design = createDesignMatrix(y)
  }

  #check if any empty column
  is.zero = which(colSums(x = (design != 0), na.rm = TRUE) == 0)
  #remove
  if(length(is.zero)>0){
    design = design[,-is.zero,drop=F]
  }

  # Pipeline ------------------------------------------------------------------#
  #Design matrix should be full rank
  if(isTRUE(limma::is.fullrank(design))){

    #Limma expects log2 data, so log the data if not already log2
    if(isFALSE(is.logged)){
      logTrace(object = logger, message = "Apply logarithmic transformation to data.", sep = "\n", add.level = TRUE, add.time = TRUE)
      x = log2(x);
    }

    #RNA-seq: Model the mean-variance relationship with precision weights
    if(identical(technology, "seq") && identical(mean.variance, "weights")){
      logTrace(object = logger, message = "Model the mean-variance relationship with precision weights.", sep = "\n", add.level = TRUE, add.time = TRUE)
      x = limma::voom(
        counts      = x,
        design      = design,
        block       = block,
        weights     = weights,
        correlation = correlation,
        span        = span,
        plot        = FALSE,
        save.plot   = FALSE
      )
    }

    #Fit the model (x has rows corresponding to genes and columns to samples)
    logDebug(object = logger, message = "Fitting a linear model for each feature given a series of observations...", sep = "", add.level = TRUE, add.time = TRUE)
    limmaFit <- limma::lmFit(
      object      = x,
      design      = design,
      method      = method,
      weights     = weights,
      ndups       = ndups,
      spacing     = spacing,
      block       = block,
      correlation = correlation
      ,...
    )
    logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)


    if(!is.null(contrasts)){
      #Compute the coefficients and the standard errors for the given set of contrasts
      logDebug(object = logger, message = "Computing estimated coefficients and standard errors for a given set of contrasts...", sep = "", add.level = TRUE, add.time = TRUE)
      limmaFit <- limma::contrasts.fit(
        fit = limmaFit,
        contrasts = contrasts
      )
      logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    }

    #Compute statistics on the model by empirical Bayes moderation of the standard errors towards a common value
    logDebug(object = logger, message = "Computing moderated t-/F-statistic, and log-odds of differential expression by empirical Bayes moderation of the standard errors towards a common value...", sep = "", add.level = TRUE, add.time = TRUE)
    trend = if(identical(technology, "seq") && identical(mean.variance, "ebayes")){TRUE}else{FALSE}
    limmaEBayes <- limma::eBayes(
      fit            = limmaFit,
      proportion     = proportion,
      stdev.coef.lim = stdev.coef.lim,
      robust         = robust,
      winsor.tail.p  = winsor.tail.p,
      trend          = trend
    )
    logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)


    #Get a list of top-ranked genes differential expressed from a previous linear model fit.
    #We adjust the p-value for multiple testing.
    logDebug(object = logger, message = "Extracting a table of the top-ranked genes from the linear model fit...", sep = "", add.level = TRUE, add.time = TRUE)
    limmaTop = limma::topTable(
      fit           = limmaEBayes,
      coef          = coef,
      number        = Inf,
      genelist      = limmaEBayes$genes,
      adjust.method = adjust.method,
      p.value       = 1,
      sort.by       = "none",
      fc            = NULL,
      lfc           = NULL,
      confint       = FALSE
    )
    logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #Extract values
    out = limmaTop$F

    if(isTRUE(is.null(out))){
      stop(paste(
        "Moderated F-statistic was not found in the dataframe returned by limma::topTable.",
        "Check 'x', 'design', 'coef', and 'contrasts' parameters."
      ))
    }

    #output
    out = list(statistic = out, significance = limmaTop$P.Value)

  } else {
    logTrace(object = logger, message = "Design matrix is not full rank.", sep = "\n", add.level = TRUE, add.time = TRUE)
    out = list(statistic = NULL, significance = NULL)
  }

  # Describe
  attr(x = out, which = "comment") = getStatisticalMethodName(id = "moderated.F")

  # End -----------------------------------------------------------------------#
  closeCon(logger)
  return(out)
}

#'Empirical Bayes Moderated t-Statistic
#'
#'@description This function implements a common workflow to fit linear models
#'and compute moderated moderated t-statistic by empirical Bayes
#'moderation of the standard errors towards a global value.
#'
#'If the input data was generated with microarray technology, the following
#'steps are executed:
#'
#'\enumerate{
#' \item linear model fitting for each feature: \code{\link[limma]{lmFit}}
#' \item compute coefficients and standard errors for a given set of contrasts: \code{\link[limma]{contrasts.fit}}
#' \item compute moderated t-statistics: \code{\link[limma]{eBayes}}
#' \item summarise the linear model fit: \code{\link[limma]{topTable}}
#'}
#'
#'If the input data was generated with RNA-seq technology, the following
#'steps are executed:
#'
#'\enumerate{
#' \item (optional) transform count data for linear modelling: \code{\link[limma]{voom}}
#' \item linear model fitting for each feature: \code{\link[limma]{lmFit}}
#' \item compute coefficients and standard errors for a given set of contrasts: \code{\link[limma]{contrasts.fit}}
#' \item compute moderated t-statistics: \code{\link[limma]{eBayes}}
#' \item summarise the linear model fit: \code{\link[limma]{topTable}}
#'}
#'
#'For complete details of each step see the manual pages of the respective
#'functions.
#'
#'@inheritParams rowEBayesStatistics
#'
#'@inherit rowEBayesStatistics return
#'
#'@inherit rowEBayesStatistics author
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define row/col size
#'nr = 20
#'nc = 20
#'
#'#Data
#'x = matrix(
#'  data = stats::rnorm(n = nr*nc),
#'  nrow = nr,
#'  ncol = nc,
#'  dimnames = list(
#'    paste0("g",seq(nr)),
#'    paste0("S",seq(nc))
#'  )
#')
#'
#'#Categorical output vector (binomial)
#'y = c(rep("wt",nc/2), rep("mut",nc/2))
#'names(y) = paste0("S",seq(nc))
#'rowEBayesStatistics(x=x,y=y)
#'
#'@export
rowModeratedT <- function(
    x,
    y = NULL,
    observations = NULL,

    #further arguments
    technology = c("array","seq"),
    is.logged  = TRUE,
    mean.variance = c("ebayes", "weights"),

    #voom
    span = 0.5,

    #lmFit
    method      = c("ls", "robust"),
    design      = NULL,
    weights     = NULL,
    ndups       = NULL,
    spacing     = NULL,
    block       = NULL,
    correlation = NULL,

    #contrasts.fit
    contrasts = NULL,

    #eBayes
    proportion     = 0.01,
    stdev.coef.lim = c(0.1, 4),
    robust         = FALSE,
    winsor.tail.p  = c(0.05, 0.1),

    #topTable
    coef          = NULL,
    adjust.method = "BH",

    #logger
    logger = NULL,
    ...
){

  # Setup ---------------------------------------------------------------------#
  technology = match.arg(technology)
  method = match.arg(method)
  mean.variance = match.arg(mean.variance)
  statistic = match.arg(statistic)

  if(isTRUE(is.null(y) && is.null(design))){stop("'design' and/or 'y' must be provided.")}

  logger = openCon(logger)
  rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");

  # Subset
  if(isTRUE(!is.null(observations))){
    x      =      x[,observations,drop=F]
    design = design[observations,,drop=F]
    block  = block[observations]
    if(isTRUE(!is.null(weights))){
      if(isTRUE(is.null(dim(weights)))){
        weights = weights[observations]
      } else {
        if(isTRUE(ncol(x)==ncol(weights))){
          weights = weights[,observations,drop=F]
        } else {stop("'ncol(weights)' different from 'ncol(x)'. Please, check your input.\n")}
      }
    }
  }

  #check design matrix
  if(!is.null(design)){
    if(!is.null(y)){
      logTrace(object = logger, message = "Check provided design matrix.", sep = "\n", add.level = TRUE, add.time = TRUE)
      #check in common
      inter = intersect(x = colnames(x), y = rownames(design))
      #update
      if(length(inter)!=ncol(x)){
        warning("There is a problem with the provided design matrix. 'y' will be used as design matrix.\n")
        design = y;
      } else {
        design = design[inter,,drop=F]
      }
    }
  } else {
    design = createDesignMatrix(y)
  }

  #check if any empty column
  is.zero = which(colSums(x = (design != 0), na.rm = TRUE) == 0)
  #remove
  if(length(is.zero)>0){
    design = design[,-is.zero,drop=F]
  }

  # Pipeline ------------------------------------------------------------------#
  #Design matrix should be full rank
  if(isTRUE(limma::is.fullrank(design))){

    #Limma expects log2 data, so log the data if not already log2
    if(isFALSE(is.logged)){
      logTrace(object = logger, message = "Apply logarithmic transformation to data.", sep = "\n", add.level = TRUE, add.time = TRUE)
      x = log2(x);
    }

    #RNA-seq: Model the mean-variance relationship with precision weights
    if(identical(technology, "seq") && identical(mean.variance, "weights")){
      logTrace(object = logger, message = "Model the mean-variance relationship with precision weights.", sep = "\n", add.level = TRUE, add.time = TRUE)
      x = limma::voom(
        counts      = x,
        design      = design,
        block       = block,
        weights     = weights,
        correlation = correlation,
        span        = span,
        plot        = FALSE,
        save.plot   = FALSE
      )
    }

    #Fit the model (x has rows corresponding to genes and columns to samples)
    logDebug(object = logger, message = "Fitting a linear model for each feature given a series of observations...", sep = "", add.level = TRUE, add.time = TRUE)
    limmaFit <- limma::lmFit(
      object      = x,
      design      = design,
      method      = method,
      weights     = weights,
      ndups       = ndups,
      spacing     = spacing,
      block       = block,
      correlation = correlation
      ,...
    )
    logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)


    if(!is.null(contrasts)){
      #Compute the coefficients and the standard errors for the given set of contrasts
      logDebug(object = logger, message = "Computing estimated coefficients and standard errors for a given set of contrasts...", sep = "", add.level = TRUE, add.time = TRUE)
      limmaFit <- limma::contrasts.fit(
        fit = limmaFit,
        contrasts = contrasts
      )
      logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    }

    #Compute statistics on the model by empirical Bayes moderation of the standard errors towards a common value
    logDebug(object = logger, message = "Computing moderated t-/F-statistic, and log-odds of differential expression by empirical Bayes moderation of the standard errors towards a common value...", sep = "", add.level = TRUE, add.time = TRUE)
    trend = if(identical(technology, "seq") && identical(mean.variance, "ebayes")){TRUE}else{FALSE}
    limmaEBayes <- limma::eBayes(
      fit            = limmaFit,
      proportion     = proportion,
      stdev.coef.lim = stdev.coef.lim,
      robust         = robust,
      winsor.tail.p  = winsor.tail.p,
      trend          = trend
    )
    logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)


    #Get a list of top-ranked genes differential expressed from a previous linear model fit.
    #We adjust the p-value for multiple testing.
    logDebug(object = logger, message = "Extracting a table of the top-ranked genes from the linear model fit...", sep = "", add.level = TRUE, add.time = TRUE)
    limmaTop = limma::topTable(
      fit           = limmaEBayes,
      coef          = coef,
      number        = Inf,
      genelist      = limmaEBayes$genes,
      adjust.method = adjust.method,
      p.value       = 1,
      sort.by       = "none",
      fc            = NULL,
      lfc           = NULL,
      confint       = FALSE
    )
    logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #Extract values
    out = limmaTop$t

    if(isTRUE(is.null(out))){
      stop(paste(
        "Moderated t-statistic was not found in the dataframe returned by limma::topTable.",
        "Check 'x', 'design', 'coef', and 'contrasts' parameters."
      ))
    }

    #output
    out = list(statistic = out, significance = limmaTop$P.Value)

  } else {
    logTrace(object = logger, message = "Design matrix is not full rank.", sep = "\n", add.level = TRUE, add.time = TRUE)
    out = list(statistic = NULL, significance = NULL)
  }

  # Describe
  attr(x = out, which = "comment") = getStatisticalMethodName(id = "moderated.t")

  # End -----------------------------------------------------------------------#
  closeCon(logger)
  return(out)
}

# Significance Analysis of Microarrays -----------------------------------------

#'Significance Analysis of Microarrays
#'
#'@description This function implements a common workflow to compute repeated
#'permutations of the input data to determine if any features are significantly
#'related to the response.
#'
#'The following steps are executed:
#'
#'\enumerate{
#' \item correlate each feature with outcome variable: \code{\link[samr]{samr}}
#' \item compute thresholds, cutpoints, and false discovery rates for SAM analysis: \code{\link[samr]{samr.compute.delta.table}}
#' \item compute SAM statistics and significance: \code{\link[samr]{samr.compute.siggenes.table}}
#'}
#'
#'@inheritParams samr::samr
#'@inheritParams samr::samr.compute.delta.table
#'@inheritParams samr::samr.compute.siggenes.table
#'@inheritParams samr::SAM
#'
#'@param observations (optional) integer vector, the indices of observations to
#'keep.
#'@param technology character string, the technology used to generate the data.
#'Available options are:
#'\describe{
#'   \item{array}{data generated with microarray technology}
#'   \item{seq}{data generated with RNA-seq technology}
#'}
#'@param logger a \code{\link{Logger}} object.
#'
#'@return A list containing two elements:
#'
#'\describe{
#'   \item{statistic}{A numeric vector, the values of the test statistic}
#'   \item{significance}{A numeric vector, the q-values of the selected test}
#'}
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define row/col size
#'nr = 10
#'nc = 20
#'
#'#Data
#'x = matrix(
#'  data = stats::rnorm(n = nr*nc),
#'  nrow = nr,
#'  ncol = nc,
#'  dimnames = list(
#'    paste0("f",seq(nr)),
#'    paste0("S",seq(nc))
#'  )
#')
#'
#'#Categorical output vector (binomial)
#'y = c(rep(1,nc/2), rep(2,nc/2))
#'names(y) = paste0("S",seq(nc))
#'rowSamStatistics(x=x,y=y)
#'
#'#Categorical output vector (multinomial)
#'y = c(rep(1,nc/4), rep(2,nc/4), rep(3,nc/2))
#'names(y) = paste0("S",seq(nc))
#'rowSamStatistics(x=x, y=y, resp.type = "Multiclass")
#'
#'@export
rowSamStatistics <- function(
    x,
    y = NULL,
    observations = NULL,

    #samr
    technology = c("array","seq"),
    geneid            = NULL,
    genenames         = NULL,
    censoring.status  = NULL,
    logged2           = FALSE,
    eigengene.number  = 1,
    resp.type = c("Quantitative","Two class unpaired","Survival","Multiclass",
                "One class", "Two class paired","Two class unpaired timecourse",
                "One class timecourse","Two class paired timecourse", "Pattern discovery"),
    s0                = NULL,
    s0.perc           = NULL,
    nperms            = 100,
    center.arrays     = FALSE,
    testStatistic     = c("standard","wilcoxon"),
    time.summary.type = c("slope","signed.area"),
    regression.method = c("standard","ranks"),
    knn.neighbors     = 10,
    random.seed       = NULL,
    #For RNA-seq
    nresamp           = 20,
    nresamp.perm      = NULL,

    #samr.compute.delta.table
    dels              = NULL,
    nvals             = 50,

    #others
    fdr.output        = 0.20,

    #Logger
    logger            = NULL
  ){
  # Setup ---------------------------------------------------------------------#
  technology        = match.arg(technology)
  resp.type         = match.arg(resp.type)
  testStatistic     = match.arg(testStatistic)
  time.summary.type = match.arg(time.summary.type)
  regression.method = match.arg(regression.method)

  xl.mode = "regular"
  xl.time = NULL
  xl.prevfit = NULL

  # Subset
  if(isTRUE(!is.null(observations))){
    x = x[,observations,drop=F]
    y = y[observations]
    censoring.status = censoring.status[observations]
  }

  rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");

  logger = openCon(logger)

  if (is.null(geneid)) {
    geneid = as.character(1:nrow(x))
  }

  genenames = rownames(x);



  # Pipeline ------------------------------------------------------------------#
  #create data object
  data = list(
    x = x,
    y = y,
    censoring.status = censoring.status,
    geneid           = geneid,
    genenames        = genenames,
    logged2          = logged2,
    eigengene.number = eigengene.number
  )

  logDebug(object = logger, message = "Correlating features with the outcome variable...", sep = "", add.level = TRUE, add.time = TRUE)
  utils::capture.output(
    samr.obj <- samr::samr(
      data              = data,
      resp.type         = resp.type,
      assay.type        = technology,
      s0                = s0,
      s0.perc           = s0.perc,
      nperms            = nperms,
      center.arrays     = center.arrays,
      testStatistic     = testStatistic,
      time.summary.type = time.summary.type,
      regression.method = regression.method,
      return.x          = TRUE,
      knn.neighbors     = knn.neighbors,
      random.seed       = random.seed,
      nresamp           = nresamp,
      nresamp.perm      = nresamp.perm
    )
  )
  logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  logDebug(object = logger, message = "Computing thresholds, cutpoints, and false discovery rates...", sep = "", add.level = TRUE, add.time = TRUE)
  utils::capture.output(
    delta.table <- samr::samr.compute.delta.table(
      samr.obj       = samr.obj,
      min.foldchange = 0,
      dels           = dels,
      nvals          = nvals
    )
  )
  logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  siggenes.table <- del <- NULL

  #Select deltas for which number of genes called significant is greater than zero
  delta.table <- delta.table[delta.table[, "# called"] > 0, , drop = FALSE]

  if (isTRUE(nrow(delta.table) > 0)) {

    oo <- which(delta.table[, "median FDR"] >= fdr.output)
    if (length(oo) > 0) {
      oo <- oo[length(oo)]
    } else {
      oo <- 1
    }
    delta.table <- delta.table[oo:nrow(delta.table), , drop = FALSE]
    del <- delta.table[1, "delta"]

    logDebug(object = logger, message = "Computing significant genes table...", sep = "", add.level = TRUE, add.time = TRUE)
    siggenes.table <- samr::samr.compute.siggenes.table(
      samr.obj         = samr.obj,
      del              = del,
      data             = data,
      delta.table      = delta.table,
      min.foldchange   = 0,
      all.genes        = TRUE,
      compute.localfdr = FALSE
    )
    logDebug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #Extract features with positive and negative correlation with the outcome
    genes.up = siggenes.table$genes.up;
    genes.lo = siggenes.table$genes.lo;

    #Bind the lists
    genes.res = rbind(genes.up, genes.lo)

    #Set rownames
    rownames(genes.res) = genes.res[,"Gene ID"]

    #Order as input
    genes.res = genes.res[genenames,,drop=FALSE]

    #Create output
    out = list(
      statistic    = round(as.numeric(genes.res[,"Score(d)"]),3),
      significance = round(as.numeric(genes.res[,"q-value(%)"])/100,3)
    )

  } else {
    logTrace(object = logger, message = "No feature was called significant.", sep = "\n", add.level = TRUE, add.time = TRUE)
    out = list(statistic = NULL, significance = NULL)
  }

  # Describe
  attr(x = out, which = "comment") = getStatisticalMethodName(id = "sam.test")
  # End -----------------------------------------------------------------------#
  closeCon(logger)
  return(out)
}
