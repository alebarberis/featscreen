# Default Values ---------------------------------------------------------------

#'Package Name
#'
#'@description This function returns the name of the package.
#'
#'@return A string containing the name of the package.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getThisPackageName <- function(){
  name = "featscreen"
  return(name)
}



#'Variability Measure Name
#'
#'@description This function returns the name of the variability measure in input.
#'
#'@param id character string, one of the supported variability measures.
#'
#'@return A character string, the name of the variability measure.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getVariabilityMeasureName <- function(
    id = c("sd", "iqr", "mad", "rsd", "efficiency", "vmr")
){
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    "sd"         = "standard deviation",
    "iqr"        = "interquartile range",
    "mad"        = "median absolute deviation",
    "rsd"        = "relative standard deviation",
    "efficiency" = "coefficient of variation squared",
    "vmr"        = "variance-to-mean ratio"
  )
  #return
  return(out)
}

#'Screening Test Statistic Name
#'
#'@description This function returns the name of the statistic used for
#'screening.
#'
#'@param id character string, one of the supported statistical techniques.
#'
#'@return A character string, the name of the statistic.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getStatisticName <- function(
    id = c(
      "cor.test",
      "pearson",
      "spearman",
      "kendall",
      "t.test",
      "t.test.equal",
      "t.test.unequal",
      "t.test.paired",
      "w.test.ranksum",
      "w.test.paired",
      "anova",
      "anova.equal",
      "anova.unequal",
      "kruskal.wallis",
      "chisq.test",
      "coxph",
      "moderated.t",
      "moderated.F",
      "sam.test",
      "missing.value",
      "above.median",
      "above.minimum",
      "median",
      "variability"
    )
){
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    # Correlations
    "cor.test"       = "t-statistic",
    "pearson"        = "t-statistic",
    "spearman"       = "t-statistic",
    "kendall"        = "t-statistic",

    # Two-sample Tests
    "t.test"         = "t-statistic",
    "t.test.equal"   = "t-statistic",
    "t.test.unequal" = "t-statistic",
    "t.test.paired"  = "t-statistic",
    "w.test.ranksum" = "U-statistic",
    "w.test.paired"  = "W-statistic",

    # Multivariate
    "anova"          = "F-statistic",
    "anova.equal"    = "F-statistic",
    "anova.unequal"  = "F-statistic",
    "kruskal.wallis" = "H-statistic",

    # Count data
    "chisq.test"     = "chisq-statistic",

    # Survival
    "coxph"          = "z-statistic",

    # Biostat
    "moderated.t"    = "t-statistic",
    "moderated.F"    = "F-statistic",
    "sam.test"       = "sam-statistic",

    # Unsupervised
    "missing.value"  = "missing value ratio",
    "above.median"   = "above-median frequency ratio",
    "above.minimum"  = "above-minimum frequency ratio",
    "median"         = "median",
    "variability"    = "variability"
  )
  #return
  return(out)
}

#'Screening Test Category
#'
#'@description This function returns the category of the statistic used for
#'screening.
#'
#'@param id character string, one of the supported statistical techniques.
#'
#'@return A character string, the name of the assigned group.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getStatisticCategory <- function(
    id = c(
      "cor.test",
      "pearson",
      "spearman",
      "kendall",
      "t.test",
      "t.test.equal",
      "t.test.unequal",
      "t.test.paired",
      "w.test.ranksum",
      "w.test.paired",
      "anova",
      "anova.equal",
      "anova.unequal",
      "kruskal.wallis",
      "chisq.test",
      "coxph",
      "moderated.t",
      "moderated.F",
      "sam.test",
      "missing.value",
      "above.median",
      "above.minimum",
      "median",
      "variability"
    )
){
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    # Correlations
    "cor.test"       = "correlation",
    "pearson"        = "correlation",
    "spearman"       = "correlation",
    "kendall"        = "correlation",

    # Two-sample Tests
    "t.test"         = "two-groups",
    "t.test.equal"   = "two-groups",
    "t.test.unequal" = "two-groups",
    "t.test.paired"  = "two-groups",
    "w.test.ranksum" = "two-groups",
    "w.test.paired"  = "two-groups",

    # Multivariate
    "anova"          = "multi-groups",
    "anova.equal"    = "multi-groups",
    "anova.unequal"  = "multi-groups",
    "kruskal.wallis" = "multi-groups",

    # Count data
    "chisq.test"     = "multi-groups",

    # Survival
    "coxph"          = "survival",

    # Biostat
    "moderated.t"    = "biostatistic",
    "moderated.F"    = "biostatistic",
    "sam.test"       = "biostatistic",

    # Unsupervised
    "missing.value"  = "missing-data",
    "above.median"   = "magnitude",
    "above.minimum"  = "magnitude",
    "median"         = "magnitude",
    "variability"    = "variability"
  )
  #return
  return(out)
}

#'Default Ranking Options
#'
#'@description This function returns the default ranking options for the the
#'statistical measure in input.
#'
#'@param id character string, one of the supported statistical techniques.
#'
#'@return A list with three elements:
#'\describe{
#'   \item{measure}{A character string indicating whether to use the statistic or the significance (where computed)}
#'   \item{order}{A character string indicating whether to sort the statistic/significance in ascending or descending order}
#'   \item{use.abs}{A logical value, whether to use the absolute values of the statistic}
#'}
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getDefaultRankingOptions <- function(
    id = c(
      "cor.test",
      "pearson",
      "spearman",
      "kendall",
      "t.test",
      "t.test.equal",
      "t.test.unequal",
      "t.test.paired",
      "w.test",
      "w.test.ranksum",
      "w.test.paired",
      "anova",
      "anova.equal",
      "anova.unequal",
      "kruskal.wallis",
      "chisq.test",
      "coxph",
      "moderated.t",
      "moderated.F",
      "sam.test",
      "missing.value",
      "above.median",
      "above.minimum",
      "median",
      "variability"
    )
){
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    # Correlations
    "cor.test"       = list(measure = "statistic", order = "descending", use.abs = TRUE),
    "pearson"        = list(measure = "statistic", order = "descending", use.abs = TRUE),
    "spearman"       = list(measure = "statistic", order = "descending", use.abs = TRUE),
    "kendall"        = list(measure = "statistic", order = "descending", use.abs = TRUE),

    # Two-sample Tests
    "t.test"         = list(measure = "statistic", order = "descending", use.abs = TRUE),
    "t.test.equal"   = list(measure = "statistic", order = "descending", use.abs = TRUE),
    "t.test.unequal" = list(measure = "statistic", order = "descending", use.abs = TRUE),
    "t.test.paired"  = list(measure = "statistic", order = "descending", use.abs = TRUE),
    "w.test"         = list(measure = "statistic", order = "ascending", use.abs = FALSE),
    "w.test.ranksum" = list(measure = "statistic", order = "ascending", use.abs = FALSE),
    "w.test.paired"  = list(measure = "statistic", order = "ascending", use.abs = FALSE),

    # Multivariate
    "anova"          = list(measure = "statistic", order = "descending", use.abs = FALSE),
    "anova.equal"    = list(measure = "statistic", order = "descending", use.abs = FALSE),
    "anova.unequal"  = list(measure = "statistic", order = "descending", use.abs = FALSE),
    "kruskal.wallis" = list(measure = "significance", order = "ascending", use.abs = FALSE),

    # Count data
    "chisq.test"     = list(measure = "significance", order = "ascending", use.abs = FALSE),

    # Survival
    "coxph"          = list(measure = "statistic", order = "descending", use.abs = TRUE),

    # Biostat
    "moderated.t"    = list(measure = "statistic", order = "descending", use.abs = TRUE),
    "moderated.F"    = list(measure = "statistic", order = "descending", use.abs = FALSE),
    "sam.test"       = list(measure = "statistic", order = "descending", use.abs = TRUE),

    # Unsupervised
    "missing.value"  = list(measure = "statistic", order = "ascending", use.abs = FALSE),
    "above.median"   = list(measure = "statistic", order = "descending", use.abs = FALSE),
    "above.minimum"  = list(measure = "statistic", order = "descending", use.abs = FALSE),
    "median"         = list(measure = "statistic", order = "descending", use.abs = FALSE),
    "variability"    = list(measure = "statistic", order = "descending", use.abs = FALSE)
  )
  return(out)
}


#'Screening Method Data Type
#'
#'@description This function returns the data type(s) the screening method in input
#'can work on.
#'
#'@return A character string or a string vector, the data types supported by the
#'screening method.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getScreeningMethodDataType <- function(
    id = c(
      "cor.test",
      "pearson",
      "spearman",
      "kendall",
      "t.test",
      "t.test.equal",
      "t.test.unequal",
      "t.test.paired",
      "w.test",
      "w.test.ranksum",
      "w.test.paired",
      "anova",
      "anova.equal",
      "anova.unequal",
      "kruskal.wallis",
      "chisq.test",
      "coxph",
      "moderated.t",
      "moderated.F",
      "sam.test",
      "missing.value",
      "above.median",
      "above.minimum",
      "median",
      "variability"
    )
){
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    # Correlations
    "cor.test"       = c("numerical"),
    "pearson"        = c("numerical"),
    "spearman"       = c("numerical"),
    "kendall"        = c("numerical"),

    # Two-sample Tests
    "t.test"         = c("categorical"),
    "t.test.equal"   = c("categorical"),
    "t.test.unequal" = c("categorical"),
    "t.test.paired"  = c("categorical"),
    "w.test"         = c("categorical"),
    "w.test.ranksum" = c("categorical"),
    "w.test.paired"  = c("categorical"),

    # Multivariate
    "anova"          = c("categorical", "multiresponse"),
    "anova.equal"    = c("categorical", "multiresponse"),
    "anova.unequal"  = c("categorical", "multiresponse"),
    "kruskal.wallis" = c("categorical", "multiresponse"),

    # Count data
    "chisq.test"     = c("categorical", "multiresponse"),

    # Survival
    "coxph"          = c("survival"),

    # Biostat
    "moderated.t"    = c("numerical", "categorical", "multiresponse"),
    "moderated.F"    = c("numerical", "categorical", "multiresponse"),
    "sam.test"       = c("numerical", "categorical", "multiresponse", "survival"),

    # Unsupervised
    "missing.value"  = c("numerical", "categorical", "multiresponse", "survival"),
    "above.median"   = c("numerical"),
    "above.minimum"  = c("numerical"),
    "median"         = c("numerical"),
    "variability"    = c("numerical")
  )
  #return
  return(out)
}

## Method Names ----------------------------------------------------------------

#'Statistical Method Name
#'
#'@description This function returns the name of the statistical method in input.
#'
#'@param id character string, one of the supported statistical techniques.
#'
#'@return A character string, the name of the statistical technique.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getStatisticalMethodName <- function(
    id = c(
      "cor.test",
      "pearson",
      "spearman",
      "kendall",
      "t.test",
      "t.test.equal",
      "t.test.unequal",
      "t.test.paired",
      "w.test",
      "w.test.ranksum",
      "w.test.paired",
      "anova",
      "anova.equal",
      "anova.unequal",
      "kruskal.wallis",
      "chisq.test",
      "coxph",
      "moderated.t",
      "moderated.F",
      "sam.test",
      "missing.value",
      "above.median",
      "above.minimum",
      "median",
      "variability"
    )
){
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    # Correlations
    "cor.test"       = "correlation coefficient t-test",
    "pearson"        = "Pearson's product moment correlation coefficient t-test",
    "spearman"       = "Spearman's rank correlation coefficient t-test",
    "kendall"        = "Kendall's rank correlation coefficient t-test",

    # Two-sample Tests
    "t.test"         = "two-sample t-test",
    "t.test.equal"   = "two-sample Student's pooled t-test",
    "t.test.unequal" = "two-sample t-test with the Welch modification to the degrees of freedom",
    "t.test.paired"  = "paired two-sample Student's t-test",
    "w.test"         = "two-sample Wilcoxon's test",
    "w.test.ranksum" = "two-sample Mann-Whitney U-test",
    "w.test.paired"  = "paired two-sample Wilcoxon signed-rank test",

    # Multivariate
    "anova"          = "one-way analysis of variance F-test",
    "anova.equal"    = "one-way analysis of variance F-test",
    "anova.unequal"  = "one-way analysis of variance F-test with Welch correction",
    "kruskal.wallis" = "Kruskal-Wallis H-test",

    # Count data
    "chisq.test"     = "Pearson's \u03C7\U00B2-test",

    # Survival
    "coxph"          = "Cox PH regression coefficient z-test",

    # Biostat
    "moderated.t"    = "empirical Bayes moderated t-test",
    "moderated.F"    = "empirical Bayes moderated F-test",
    "sam.test"       = "significant analysis of microarrays permutation test",

    # Unsupervised
    "missing.value"  = "missing value ratio",
    "above.median"   = "above-median frequency ratio",
    "above.minimum"  = "above-minimum frequency ratio",
    "median"         = "median value",
    "variability"    = "variability"
  )
  #return
  return(out)
}

#'Multiresponse Aggregation Method Name
#'
#'@description This function returns the name of the multi-response aggregation
#'method in input.
#'
#'@param id character string, one of the supported multi-response aggregation methods.
#'
#'@return A character string, the name of the multi-response aggregation method.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getMultiresponseAggregationMethodName <- function(
    id = c("max", "min", "avg", "sum", "idx")
){
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    "max" = "maximum value across responses",
    "min" = "minimum value across responses",
    "avg" = "mean value across responses",
    "sum" = "sum of responses",
    "idx" = "user-selected response"
  )
  #return
  return(out)
}

#'Selection Method Name
#'
#'@description This function returns the name of the selection method in input.
#'
#'@param id character string, one of the supported selecting methods.
#'
#'@return A character string, the name of the selecting method.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getSelectionMethodName <- function(
    id = c("cutoff", "rank", "percentile", "fpr", "fdr")
){
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    "cutoff"     = "selection by cutoff",
    "rank"       = "selection by ranking",
    "percentile" = "selection by top percentile",
    "fpr"        = "selection by false positive rate",
    "fdr"        = "selection by false discovery rate"
  )
  #return
  return(out)
}

## Function Names --------------------------------------------------------------


#'Statistical Function Name
#'
#'@description This function returns the name of the statistical function in input.
#'
#'@param id character string, one of the supported statistical techniques.
#'
#'@return A character string, the name of the statistical function.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getStatisticalFunctionName <- function(
    id = c(
      "cor.test",
      "pearson",
      "spearman",
      "kendall",
      "t.test",
      "t.test.equal",
      "t.test.unequal",
      "t.test.paired",
      "w.test",
      "w.test.ranksum",
      "w.test.paired",
      "anova",
      "anova.equal",
      "anova.unequal",
      "kruskal.wallis",
      "chisq.test",
      "coxph",
      "moderated.t",
      "moderated.F",
      "sam.test",
      "missing.value",
      "above.median",
      "above.minimum",
      "median",
      "variability"
    )
){
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    "cor.test"       = "rowCor",
    "pearson"        = "rowPearsonCor",
    "spearman"       = "rowSpearmanCor",
    "kendall"        = "rowKendallCor",
    "t.test"         = "rowTwoSampleT",
    "t.test.equal"   = "rowEqualVarT",
    "t.test.unequal" = "rowUnequalVarT",
    "t.test.paired"  = "rowPairedT",
    "w.test"         = "rowTwoSampleWilcoxonT",
    "w.test.ranksum" = "rowWilcoxonT",
    "w.test.paired"  = "rowPairedWilcoxonT",
    "anova"          = "rowOneWayAnova",
    "anova.equal"    = "rowEqualVarOneWayAnova",
    "anova.unequal"  = "rowUnequalVarOneWayAnova",
    "kruskal.wallis" = "rowKruskalWallis",
    "chisq.test"     = "rowPearsonChiSq",
    "coxph"          = "rowCoxPH",
    "moderated.t"    = "rowModeratedT",
    "moderated.F"    = "rowModeratedOneWayAnova",
    "sam.test"       = "rowSamStatistics",
    "missing.value"  = "rowMissingValueRatio",
    "above.median"   = "rowAboveMedianFreqRatio",
    "above.minimum"  = "rowAboveMinFreqRatio",
    "median"         = "rowMedians",
    "variability"    = "rowVariability"
  )
  #return
  return(out)
}

#'Selection Function Name
#'
#'@description This function returns the name of the selection function in input.
#'
#'@param id character string, one of the supported selection functions.
#'
#'@return A character string, the name of the selection function.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
getSelectionFunctionName <- function(
    id = c("cutoff", "rank", "percentile", "fpr", "fdr")
){
  #match
  id = match.arg(id)
  #switch
  out = switch(
    id,
    "cutoff"     = "selectByCutoff",
    "rank"       = "selectByRanking",
    "percentile" = "selectByPercentile",
    "fpr"        = "selectByFpr",
    "fdr"        = "selectByFdr"
  )
  #return
  return(out)
}

# File and Connection -----------------------------------------------------

#'Connection Utility Functions
#'
#'@description Functions to check existence, create, open, and close
#'connections.
#'
#'@name connectionUtilityFunctions
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
NULL

#'Connection Utility Functions
#'@describeIn connectionUtilityFunctions Checks the existence of a file path
#'
#'@param path character string, the file path
#'@return A logical value, whether the path to the file exists.
#'@keywords internal
filePathExists <- function(path = NULL){
  if(isTRUE(!missing(path) && !is.null(path) && length(path)>0 && dir.exists(dirname(path)))){
    bool = TRUE;
  } else {
    bool = F;
  }
  return(bool);
}

#'Connection Utility Functions
#'
#'@describeIn connectionUtilityFunctions Checks if \code{con}
#'is a connection.
#'@param con object to be tested
#'@return Returns \code{TRUE} or \code{FALSE} depending on whether
#'its argument is of \code{connection} class or not.
#'@keywords internal
is.connection <- function(con){
  return(isTRUE(sum(grepl(pattern = "connection", x = class(con)))==1))
}

#'Connection Utility Functions
#'
#'@describeIn connectionUtilityFunctions Checks the existence of a
#'file path and returns an opened connection.
#'
#'@param path character string, the file path
#'@return An opened connection.
#'@keywords internal
checkFilePathAndOpenConnection <- function(path = NULL){
  if(isTRUE(filePathExists(path))){
    #Open connection to log and warning files
    con = file(description = path, open = "a");
  } else {
    con = NULL;
    # con = character();
  }
  return(con);
}


#'Connection Utility Functions
#'
#'@describeIn connectionUtilityFunctions Returns \code{TRUE} or
#'\code{FALSE} depending on whether its argument is an opened
#'\code{connection}.
#'
#'@param con object to be tested
#'@return Returns \code{TRUE} or \code{FALSE} depending on whether
#'its argument is an opened \code{connection}.
#'@keywords internal
isOpenConnection <- function(con){

  out = FALSE;

  if(isTRUE(!missing(con) && !is.null(con))){
    if(isTRUE((sum(grepl(pattern = "connection", x = class(con)))==1) && isOpen(con = con, rw = ""))){
      out=TRUE;
    }
  }

  return(out);
}


#'Connection Utility Functions
#'
#'@describeIn connectionUtilityFunctions Closes an
#'opened connection.
#'
#'@param con object to be tested
#'
#'@keywords internal
checkConAndClose <- function(con){
  if(isTRUE(isOpenConnection(con))){
    close(con);
  }
}

#'Connection Utility Functions
#'
#'@describeIn connectionUtilityFunctions Closes an
#'opened connection.
#'
#'@param object a \code{\link{Logger}}
#'
#'@keywords internal
closeCon <- function(object){
  con = getCon(object = object)
  con = checkConAndClose(con = con)
}

#'Connection Utility Functions
#'
#'@describeIn connectionUtilityFunctions Opens a
#'connection.
#'
#'@param object a \code{\link{Logger}}
#'
#'@keywords internal
openCon <- function(object){
  fpath  = getPath(object = object)
  con    = checkFilePathAndOpenConnection(path = fpath)
  object = setCon(object = object, value = con)
  return(object)
}

# Directory ---------------------------------------------------------------

#'Check the existence of a directory
#'
#'@description This function checks the existence of a directory in input.
#'See the **Details** section below for further information.
#'
#'@param path character string, a directory path
#'
#'@return \code{TRUE} if \code{output} is a path to a directory,
#'\code{FALSE} otherwise.
#'
#'@details The function checks that the path exists.
#'If not, it creates the entire path by using
#'\code{\link[base]{dir.create}} with \code{recursive = TRUE}.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
dirExists <- function(path){
  if(!missing(path) && !is.null(path)){hasDir = T} else {hasDir = F}
  if(hasDir && !dir.exists(path)){dir.create(path, recursive = TRUE)}

  return(hasDir)
}

# Other Functions --------------------------------------------------------------
validateFeatureMatrix <- function(x){
  if(missing(x) || is.null(x) || !is.matrix(x) || !is.data.frame(x)){
    stop("Need to specify a matrix or a data.frame where the rows represent the observations and the columns the features.")
  }

  if(sum(!is.na(x))==0){stop("Input is full of NAs. Check your data.")}
}

#'Combine multi-response
#'
#'@description If screened features are from multi-response data,
#'this function allows to adopt different strategies.
#'
#'@param x \code{matrix} containing the values to combine.
#'@param multi character string indicating what to do when \code{x} has multiple
#'columns.
#'
#'Available options are:
#'\describe{
#'   \item{\code{"max"}}{the maximum value across responses is kept}
#'   \item{\code{"min"}}{the minimum value across responses is kept}
#'   \item{\code{"avg"}}{values are averaged}
#'   \item{\code{"sum"}}{values are summed up}
#'   \item{\code{"idx"}}{return the column indicated by \code{idx}}
#'}
#'@param idx (optional) integer value or character string indicating the
#'column of \code{x} to keep.
#'
#'@return A vector.
#'
#'@author Alessandro Barberis
#'
#'@export
multiresponse <- function(
    x,
    multi = c("max", "min", "avg", "sum", "idx"),
    idx = NULL
  ){

  #multi
  multi = match.arg(multi)

  #n output
  n = ncol(x)
  cnames <- colnames(x)

  #check if multi-response
  is.multi = !is.null(n)

  if(is.multi){
    text = comment(x)
    if(n > 1){
      if(identical(multi, "idx")){

        stopifnot("'idx' must be an integer value or a character string indicating a column of 'x'. Please, check your input.\n" =
                    !is.null(x) && (is.integer(idx) || is.character(idx)) )

        if(isTRUE(is.character(idx))){
          idx = match(x = idx, table = cnames)
        }

        stopifnot("'idx' must indicate a column of 'x'. Please, check your input.\n" =
                    !is.na(idx) && idx > 0 && idx <= n)

        #select response
        x = x[,idx]

      }else{
        # Compute
        x = switch(
          multi,
          'max' = matrixStats::rowMaxs(x = x, na.rm = T),
          'min' = matrixStats::rowMins(x = x, na.rm = T),
          'avg' = matrixStats::rowMeans2(x = x, na.rm = T),
          'sum' = matrixStats::rowSums2(x = x, na.rm = T)
        )
      }
    } else {
      #make sure output is vector
      x = stats::setNames(object = as.vector(x), nm = rownames(x))
    }
    attr(x = x, which = 'comment') = text
  }

  return(x)
}


#'Combine multi-response
#'
#'@description If screened features are from multi-response data,
#'this function allows to adopt different strategies.
#'
#'@param screening.text character string containing the summary of the screening.
#'@param screening.method character string indicating the screening method id.
#'@param selection.text character string containing the summary of the selection.
#'@param selection.method character string indicating the selection method id.
#'
#'@return A character string.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
combineSummaryText <- function(
    screening.text,
    screening.method = c(
      "cor.test",
      "pearson",
      "spearman",
      "kendall",
      "t.test",
      "t.test.equal",
      "t.test.unequal",
      "t.test.paired",
      "w.test",
      "w.test.ranksum",
      "w.test.paired",
      "anova",
      "anova.equal",
      "anova.unequal",
      "kruskal.wallis",
      "chisq.test",
      "coxph",
      "moderated.t",
      "moderated.F",
      "sam.test",
      "missing.value",
      "above.median",
      "above.minimum",
      "median",
      "variability"
    ),
    selection.text,
    selection.method = c(
      "cutoff",
      "rank",
      "percentile",
      "fpr",
      "fdr")
){
  screening.method = match.arg(screening.method)
  selection.method = match.arg(selection.method)


  if(isTRUE(screening.method %in% listAvailableSupervisedScreeningMethodIds())){
    linking.text = switch(
      selection.method,
      "cutoff"     = "on the statistic of the",
      "rank"       = "on the statistic of the",
      "percentile" = "of the highest statistics of the",
      "fpr"        = "of the",
      "fdr"        = "of the"
    )
  } else {
    linking.text = switch(
      selection.method,
      "cutoff"     = "on the",
      "rank"       = "on the statistic of the",
      "percentile" = "of the",
      "fpr"        = "of the",
      "fdr"        = "of the"
    )
  }
  out = paste(selection.text, linking.text, paste0(screening.text, "."))
  return(out)
}


#'Default Ranking
#'
#'@description Ranks output of statistical method used for screening.
#'
#'@param x output of the statistical function used for screening.
#'@param id character string indicating statistical method used for screening.
#'@param multi character string indicating what to do in case of multi-response
#'(available options are \code{"max"}, \code{"min"}, \code{"avg"}, \code{"sum"})
#'or integer indicating the column of \code{x} to keep.
#'
#'@return The ordering index vector.
#'
#'@author Alessandro Barberis
#'
#'@export
defaultRanking <- function(
    x,
    id = c(
      "cor.test",
      "pearson",
      "spearman",
      "kendall",
      "t.test",
      "t.test.equal",
      "t.test.unequal",
      "t.test.paired",
      "w.test",
      "w.test.ranksum",
      "w.test.paired",
      "anova",
      "anova.equal",
      "anova.unequal",
      "kruskal.wallis",
      "chisq.test",
      "coxph",
      "moderated.t",
      "moderated.F",
      "sam.test",
      "missing.value",
      "above.median",
      "above.minimum",
      "median",
      "variability"
    ),
    multi
  ){

  # Check
  stopifnot("'multi' must be a supported value. Please, check your input.\n" = is.numeric(multi) ||
              (multi %in% c("max", "min", "avg", "sum")))

  id = match.arg(id)


  # Coerce
  if(is.numeric(multi)){
    idx = as.integer(multi)
    multi = "idx"
  } else {idx = 0}

  # Get ranking options
  rnkOpts    = getDefaultRankingOptions(id = id)
  measure    = rnkOpts$measure
  decreasing = ifelse(test = identical(rnkOpts$order, "descending"), yes = TRUE, no = FALSE)
  use.abs    = rnkOpts$use.abs

  # Get measure
  if(isTRUE(is.list(x))){
    x = x[[measure]]
  }

  # Combine multi-measure
  x = multiresponse(x = x, multi = multi, idx = idx)

  # Use absolute values?
  if(isTRUE(use.abs)){
    x = abs(x = x)
  }

  # Order
  index = sort.int(
    x            = x,
    na.last      = T,
    decreasing   = decreasing,
    index.return = T,
    method       = "radix"
  )$ix

  return(index)
}

# List Functions ---------------------------------------------------------------

## Available Screening Methods -------------------------------------------------

#'Available Unsupervised Screening Method Ids
#'
#'@description This function returns the ids of the currently available methods
#'for unsupervised screening.
#'
#'@return A vector containing the screening method identifiers.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
listAvailableUnsupervisedScreeningMethodIds <- function(){
  #method ids
  out = c(
    "missing.value",
    "above.median",
    "above.minimum",
    "median",
    "variability"
  )

  return(out)
}

#'Available Supervised Screening Method Ids
#'
#'@description This function returns the ids of the currently available methods
#'for supervised screening.
#'
#'@param show.wrapper logical indicating whether to return the ids for the
#'supervised screening functions with or without wrapper functions.
#'
#'@return A vector containing the screening method identifiers.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
listAvailableSupervisedScreeningMethodIds <- function(show.wrapper = FALSE){

  #method ids
  out = c(
    "cor.test",
    "pearson",
    "spearman",
    "kendall",
    "t.test",
    "t.test.equal",
    "t.test.unequal",
    "t.test.paired",
    "w.test",
    "w.test.ranksum",
    "w.test.paired",
    "anova",
    "anova.equal",
    "anova.unequal",
    "kruskal.wallis",
    "chisq.test",
    "coxph",
    "moderated.t",
    "moderated.F",
    "sam.test"
  )
  #check
  if(isFALSE(show.wrapper)){
    out = setdiff(x = out, y = c("cor.test", "t.test", "w.test", "anova"))
  }

  return(out)
}

#'Available Screening Method IDs
#'
#'@description This function returns the currently available
#'random sampling methods.
#'
#'@param x character string indicating whether to return all screening methods
#'(`all`), the unsupervised screening methods (`unsupervised`) or the supervised
#'screening methods (`supervised`).
#'@inheritParams listAvailableSupervisedScreeningMethodIds
#'
#'@return A vector containing the screening method identifiers.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
listAvailableScreeningMethodIds <- function(
    x = c('all', 'unsupervised', 'supervised'),
    show.wrapper = FALSE
  ){
  x = match.arg(x)
  #method ids
  out = switch(
    x,
    'all' = c(
      listAvailableUnsupervisedScreeningMethodIds(),
      listAvailableSupervisedScreeningMethodIds(show.wrapper = show.wrapper)
    ),
    'unsupervised' = listAvailableUnsupervisedScreeningMethodIds(),
    'supervised'  = listAvailableSupervisedScreeningMethodIds()
  )

  return(out)
}


#'Available Screening Methods
#'
#'@description This function returns the supported data types for each
#'available screening method.
#'
#'@inheritParams listAvailableScreeningMethodIds
#'@param simplify logical indicating whether to format the data for readability.
#'
#'@return A data frame with five columns:
#'
#'\describe{
#'\item{id}{the id of the screening method, to be used in the function calls}
#'\item{numerical}{whether the method works with numerical data}
#'\item{categorical}{whether the method works with categorical data}
#'\item{multiresponse}{whether the method works with multiresponse data}
#'\item{survival}{whether the method works with survival data}
#'}
#'
#'@details If \code{simplify = FALSE}, the values of the table are returned as
#'logical (i.e., TRUE and FALSE).
#'If \code{simplify = TRUE}, the values of the table are returned as 'x' (meaning
#'the method works with the indicated data type) and ''.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
listScreeningMethodDataTypes <- function(
    x = c('all', 'unsupervised', 'supervised'),
    show.wrapper = FALSE,
    simplify = FALSE
  ){
  x = match.arg(x)
  # Method ids
  ids = listAvailableScreeningMethodIds(x=x,show.wrapper=show.wrapper)
  # Method data types
  dt = sapply(X = ids, FUN = getScreeningMethodDataType)
  # All types
  all.types = c("numerical", "categorical", "multiresponse", "survival")
  # Fill
  out = sapply(X = dt, FUN = `%in%`, x = all.types)
  out = t(out)
  colnames(out) = all.types
  # Create Output
  out = data.frame(
    id = ids,
    out,
    row.names = NULL,
    stringsAsFactors = F
  )
  if(isTRUE(simplify)){
    out[out==TRUE]='x'
    out[out==FALSE]=''
  }
  return(out)
}

#'Available Screening Functions
#'
#'@description This function returns the currently available
#'screening functions.
#'
#'@inheritParams listAvailableScreeningMethodIds
#'
#'@return A data frame with two columns:
#'
#'\describe{
#'\item{id}{the id of the screening function, to be used in the function calls}
#'\item{name}{the name of the screening function}
#'}
#'
#'@author Alessandro Barberis
#'
#'@examples
#'
#'#List available methods
#'listAvailableScreeningFunctions()
#'
#'@export
listAvailableScreeningFunctions <- function(
    x = c('all', 'unsupervised', 'supervised'),
    show.wrapper = FALSE
){
  x = match.arg(x)
  #function ids
  ids = listAvailableScreeningMethodIds(x=x,show.wrapper=show.wrapper)
  #function names
  nm = sapply(X = ids, FUN = getStatisticalFunctionName)
  #output
  out = data.frame(
    id = ids,
    name = nm,
    row.names = NULL,
    stringsAsFactors = F
  )
  return(out)
}


#'Available Screening Functions
#'
#'@description This function returns the internally given category for each
#'available screening method.
#'
#'@inheritParams listAvailableScreeningMethodIds
#'
#'@return A data frame with two columns:
#'
#'\describe{
#'\item{id}{the id of the screening function, to be used in the function calls}
#'\item{category}{the name of the internally given category}
#'}
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
listScreeningMethodCategories <- function(
    x = c('all', 'unsupervised', 'supervised'),
    show.wrapper = FALSE
){
  x = match.arg(x)
  categories = c("correlation", "two-groups", "multi-groups",
                 "chisq-statistic", "survival", "biostatistic",
                 "missing-data", "magnitude", "variability")
  #function ids
  ids = listAvailableScreeningMethodIds(x=x,show.wrapper=show.wrapper)
  #function names
  nm = sapply(X = ids, FUN = getStatisticCategory)
  #output
  out = data.frame(
    id = ids,
    category = nm,
    row.names = NULL,
    stringsAsFactors = F
  )
  #give order
  out$category = factor(x = out$category, levels = intersect(categories, out$category)
  )
  return(out)
}

#'Available Screening Methods
#'
#'@description This function returns the currently available
#'screening methods.
#'
#'@inheritParams listScreeningMethodDataTypes
#'@param show.data.type logical indicating whether to show the supported data types.
#'@param show.function logical indicating whether to show the function name.
#'@param show.category logical indicating whether to show the internally given category.
#'
#'@return A data frame with at least two columns:
#'
#'\describe{
#'\item{id}{the id of the screening method, to be used in the function calls}
#'\item{name}{the name of the screening method}
#'}
#'
#'@author Alessandro Barberis
#'
#'@examples
#'
#'#List available methods
#'listAvailableScreeningMethods()
#'
#'#List available methods and supported data types
#'listAvailableScreeningMethods(show.data.type = TRUE, simplify = TRUE)
#'
#'#List available methods and function names
#'listAvailableScreeningMethods(show.function = TRUE)
#'
#'@export
listAvailableScreeningMethods <- function(
    x = c('all', 'unsupervised', 'supervised'),
    show.wrapper = FALSE,
    show.data.type = FALSE,
    simplify = FALSE,
    show.function = FALSE,
    show.category = FALSE
  ){
  x = match.arg(x)
  #method ids
  ids = listAvailableScreeningMethodIds(x=x,show.wrapper=show.wrapper)
  #method names
  nm = sapply(X = ids, FUN = getStatisticalMethodName)
  #output
  out = data.frame(
    id = ids,
    name = nm,
    row.names = NULL,
    stringsAsFactors = F
  )
  if(show.function){
    out = merge(
      x = out,
      y = listAvailableScreeningFunctions(show.wrapper=show.wrapper),
      by = 'id'
    )
    colnames(out) = c("id", "name", "function")
  }
  if(show.data.type){
    out = merge(
      x = out,
      y = listScreeningMethodDataTypes(x=x,simplify=simplify,show.wrapper=show.wrapper),
      by = 'id'
    )
  }
  if(show.category){
    out = merge(
      x = listScreeningMethodCategories(x=x,show.wrapper=show.wrapper),
      y = out,
      by = 'id'
    )
  }
  rownames(out) = NULL
  return(out)
}

## Available Selection Methods -------------------------------------------------

#'Available Selection Method Ids
#'
#'@description This function returns the ids of the currently available methods
#'for feature selection.
#'
#'@return A vector containing the selecting method identifiers.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
listAvailableSelectionMethodIds <- function(){
  #method ids
  out = c("cutoff", "rank", "percentile", "fpr", "fdr")

  return(out)
}

#'Available Selection Functions
#'
#'@description This function returns the currently available
#'selection function names.
#'
#'@return A data frame with two columns:
#'
#'\describe{
#'\item{id}{the id of the selection method, to be used in the function calls}
#'\item{name}{the name of the selection function}
#'}
#'
#'@author Alessandro Barberis
#'
#'@examples
#'
#'#List available methods
#'listAvailableSelectionFunctions()
#'
#'@export
listAvailableSelectionFunctions <- function(){
  #method ids
  ids = listAvailableSelectionMethodIds()
  #method names
  nm = sapply(X = ids, FUN = getSelectionFunctionName)
  #output
  out = data.frame(
    id = ids,
    name = nm,
    row.names = NULL,
    stringsAsFactors = F
  )
  return(out)
}


#'Available Selection Methods
#'
#'@description This function returns the currently available
#'selection methods.
#'
#'@param show.function logical indicating whether to show the related function name.
#'
#'@return A data frame with two columns by default:
#'
#'\describe{
#'\item{id}{the id of the selection method, to be used in the function calls}
#'\item{name}{the name of the selection method}
#'}
#'
#'If \code{show.function = TRUE}, a further column containing the function name
#'is added:
#'
#'\describe{
#'\item{id}{the id of the selection method, to be used in the function calls}
#'\item{name}{the name of the selection method}
#'\item{function}{the name of the selection function}
#'}
#'
#'@author Alessandro Barberis
#'
#'@examples
#'
#'#List available methods
#'listAvailableSelectionMethods()
#'
#'#List available methods and related functions
#'listAvailableSelectionMethods(show.function = TRUE)
#'
#'@export
listAvailableSelectionMethods <- function(show.function = FALSE){
  #method ids
  ids = listAvailableSelectionMethodIds()
  #method names
  nm = sapply(X = ids, FUN = getSelectionMethodName)
  #output
  out = data.frame(
    id = ids,
    name = nm,
    row.names = NULL,
    stringsAsFactors = F
  )
  if(show.function){
    out = merge(
      x = out,
      y = listAvailableSelectionFunctions(),
      by = 'id'
    )
    colnames(out) = c("id", "name", "function")
  }
  return(out)
}


## Default Ranking Options -----------------------------------------------------

#'Default Ranking Options
#'
#'@description This function returns the default ranking options for the statistics
#'of the screening method in input.
#'
#'@inheritParams listScreeningMethodDataTypes
#'
#'@return A data frame with five columns:
#'
#'\describe{
#'\item{id}{the id of the screening method, to be used in the function calls}
#'\item{statistic}{the statistic associated with the screening method}
#'\item{measure}{the measure to use for ranking, whether to use the statistic or the significance (where computed)}
#'\item{order}{whether to sort the measure in ascending or descending order for ranking}
#'\item{value}{whether to use absolute values for ranking}
#'}
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
listDefaultRankingOption <- function(
    id = c(
      "cor.test",
      "pearson",
      "spearman",
      "kendall",
      "t.test",
      "t.test.equal",
      "t.test.unequal",
      "t.test.paired",
      "w.test",
      "w.test.ranksum",
      "w.test.paired",
      "anova",
      "anova.equal",
      "anova.unequal",
      "kruskal.wallis",
      "chisq.test",
      "coxph",
      "moderated.t",
      "moderated.F",
      "sam.test",
      "missing.value",
      "above.median",
      "above.minimum",
      "median",
      "variability"
    )
  ){
  # Match
  id = match.arg(id)
  # Get statistic name
  statisticName = getStatisticName(id = id)
  # Get ranking options
  rnkOpts = getDefaultRankingOptions(id = id)
  # Create output
  out = data.frame(
    id = id,
    statistic = statisticName,
    measure = rnkOpts$measure,
    order = rnkOpts$order,
    value = ifelse(test = rnkOpts$use.abs, yes = "absolute", no = "raw"),
    row.names = NULL,
    stringsAsFactors = F
  )
  return(out)
}



#'Default Ranking Options
#'
#'@description This function returns the default ranking options for the statistics
#'of the screening methods.
#'
#'@inherit listDefaultRankingOption return
#'
#'@author Alessandro Barberis
#'
#'@examples
#'
#'#List available methods
#'listDefaultRankingOptions()
#'
#'@export
listDefaultRankingOptions <- function(){
  #method ids
  ids = listAvailableScreeningMethodIds()
  #method ranking options
  out = t(lapply(X = ids, FUN = listDefaultRankingOption))
  #create data frame
  out = do.call(rbind, out)
  return(out)
}
