#'@include 1-log-functions.R 2-stats-functions.R 3-stats-test-functions.R 4-biostats-test-functions.R 5-selection-functions.R 6-featscreen-class.R
NULL

# Get Statistical Functions ----------------------------------------------------

#'Get Statistical Function
#'
#'@description This function is a dispatcher for the statistical function selected
#'in input.
#'
#'@param id character string, one of the supported statistical techniques.
#'
#'@return A statistical function:
#'
#'\describe{
#' \item{\code{"cor.test"      }}{\code{\link{rowCor}}}
#' \item{\code{"pearson"       }}{\code{\link{rowPearsonCor}}}
#' \item{\code{"spearman"      }}{\code{\link{rowSpearmanCor}}}
#' \item{\code{"kendall"       }}{\code{\link{rowKendallCor}}}
#' \item{\code{"t.test"        }}{\code{\link{rowTwoSampleT}}}
#' \item{\code{"t.test.equal"  }}{\code{\link{rowEqualVarT}}}
#' \item{\code{"t.test.unequal"}}{\code{\link{rowUnequalVarT}}}
#' \item{\code{"t.test.paired" }}{\code{\link{rowPairedT}}}
#' \item{\code{"w.test"        }}{\code{\link{rowTwoSampleWilcoxonT}}}
#' \item{\code{"w.test.ranksum"}}{\code{\link{rowWilcoxonT}}}
#' \item{\code{"w.test.paired" }}{\code{\link{rowPairedWilcoxonT}}}
#' \item{\code{"anova"         }}{\code{\link{rowOneWayAnova}}}
#' \item{\code{"anova.equal"   }}{\code{\link{rowEqualVarOneWayAnova}}}
#' \item{\code{"anova.unequal" }}{\code{\link{rowUnequalVarOneWayAnova}}}
#' \item{\code{"kruskal.wallis"}}{\code{\link{rowKruskalWallis}}}
#' \item{\code{"chisq.test"    }}{\code{\link{rowPearsonChiSq}}}
#' \item{\code{"coxph"         }}{\code{\link{rowCoxPH}}}
#' \item{\code{"moderated.t"   }}{\code{\link{rowModeratedT}}}
#' \item{\code{"moderated.F"   }}{\code{\link{rowModeratedOneWayAnova}}}
#' \item{\code{"sam.test"      }}{\code{\link{rowSamStatistics}}}
#' \item{\code{"missing.value" }}{\code{\link{rowMissingValueRatio}}}
#' \item{\code{"above.median"  }}{\code{\link{rowAboveMedianFreqRatio}}}
#' \item{\code{"above.minimum" }}{\code{\link{rowAboveMinFreqRatio}}}
#' \item{\code{"median"        }}{\code{\link{rowMedians}}}
#' \item{\code{"variability"   }}{\code{\link{rowVariability}}}
#'}
#'
#'@author Alessandro Barberis
#'
#'@examples
#'f = getStatFunction(id = 'cor.test')
#'
#'@export
getStatFunction <- function(
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

  #match arg
  id = match.arg(id)

  #get function
  f = switch(
    id,
    "cor.test"       = rowCor,
    "pearson"        = rowPearsonCor,
    "spearman"       = rowSpearmanCor,
    "kendall"        = rowKendallCor,
    "t.test"         = rowTwoSampleT,
    "t.test.equal"   = rowEqualVarT,
    "t.test.unequal" = rowUnequalVarT,
    "t.test.paired"  = rowPairedT,
    "w.test"         = rowTwoSampleWilcoxonT,
    "w.test.ranksum" = rowWilcoxonT,
    "w.test.paired"  = rowPairedWilcoxonT,
    "anova"          = rowOneWayAnova,
    "anova.equal"    = rowEqualVarOneWayAnova,
    "anova.unequal"  = rowUnequalVarOneWayAnova,
    "kruskal.wallis" = rowKruskalWallis,
    "chisq.test"     = rowPearsonChiSq,
    "coxph"          = rowCoxPH,
    "moderated.t"    = rowModeratedT,
    "moderated.F"    = rowModeratedOneWayAnova,
    "sam.test"       = rowSamStatistics,
    "missing.value"  = rowMissingValueRatio,
    "above.median"   = rowAboveMedianFreqRatio,
    "above.minimum"  = rowAboveMinFreqRatio,
    "median"         = rowMedians,
    "variability"    = rowVariability
  )

  #return
  return(f)
}


# Feature Screening ------------------------------------------------------------


#'Feature Screening
#'
#'@description This function computes a statistical measure for each feature
#'in input. In case of multi-response data, the screening statistics are then
#'combined as defined by \code{"multi"}. Finally, the features to keep are
#'obtained via the chosen selecting method as indicated by \code{select.by}.
#'
#'See the **Details** section below for further information.
#'
#'@param x \code{matrix} or \code{data.frame}, where rows are features and columns are observations.
#'@param g (optional) vector or factor object giving the group for the corresponding
#'elements of \code{x}.
#'@param y numeric vector of data values having the same length as \code{ncol(x)}
#'or \code{data.frame} with two columns, \code{time} and \code{status}.
#'@param method character string, one of the supported screening techniques.
#'@param ... further arguments to screening function.
#'@param multi character string indicating what to do in case of multi-response.
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
#'@param select.by character string indicating the selecting method.
#'Available options are:
#'\describe{
#'   \item{\code{"cutoff"}}{selection by cutoff}
#'   \item{\code{"rank"}}{selection by ranking}
#'   \item{\code{"percentile"}}{selection by top percentile}
#'   \item{\code{"fpr"}}{selection by false positive rate}
#'   \item{\code{"fdr"}}{selection by false discovery rate}
#'}
#'@param select.args (optional) named list, arguments to be passed to the
#'selecting function.
#'
#'@return An object of class `featscreen`.
#'
#'@details This function uses one of the selected screening technique to compute
#'a statistical measure for each feature.
#'
#'See the following functions for each specific implementation:
#'
#'\describe{
#' \item{\code{"cor.test"      }}{\code{\link{rowCor}}}
#' \item{\code{"pearson"       }}{\code{\link{rowPearsonCor}}}
#' \item{\code{"spearman"      }}{\code{\link{rowSpearmanCor}}}
#' \item{\code{"kendall"       }}{\code{\link{rowKendallCor}}}
#' \item{\code{"t.test"        }}{\code{\link{rowTwoSampleT}}}
#' \item{\code{"t.test.equal"  }}{\code{\link{rowEqualVarT}}}
#' \item{\code{"t.test.unequal"}}{\code{\link{rowUnequalVarT}}}
#' \item{\code{"t.test.paired" }}{\code{\link{rowPairedT}}}
#' \item{\code{"w.test"        }}{\code{\link{rowTwoSampleWilcoxonT}}}
#' \item{\code{"w.test.ranksum"}}{\code{\link{rowWilcoxonT}}}
#' \item{\code{"w.test.paired" }}{\code{\link{rowPairedWilcoxonT}}}
#' \item{\code{"anova"         }}{\code{\link{rowOneWayAnova}}}
#' \item{\code{"anova.equal"   }}{\code{\link{rowEqualVarOneWayAnova}}}
#' \item{\code{"anova.unequal" }}{\code{\link{rowUnequalVarOneWayAnova}}}
#' \item{\code{"kruskal.wallis"}}{\code{\link{rowKruskalWallis}}}
#' \item{\code{"chisq.test"    }}{\code{\link{rowPearsonChiSq}}}
#' \item{\code{"coxph"         }}{\code{\link{rowCoxPH}}}
#' \item{\code{"moderated.t"   }}{\code{\link{rowModeratedT}}}
#' \item{\code{"moderated.F"   }}{\code{\link{rowModeratedOneWayAnova}}}
#' \item{\code{"sam.test"      }}{\code{\link{rowSamStatistics}}}
#' \item{\code{"missing.value" }}{\code{\link{rowMissingValueRatio}}}
#' \item{\code{"above.median"  }}{\code{\link{rowAboveMedianFreqRatio}}}
#' \item{\code{"above.minimum" }}{\code{\link{rowAboveMinFreqRatio}}}
#' \item{\code{"median"        }}{\code{\link{rowMedians}}}
#' \item{\code{"variability"   }}{\code{\link{rowVariability}}}
#'}
#'
#'In case of multi-response data, the screening statistics are then
#'combined by using the \code{\link{multiresponse}} function.
#'
#'Finally, the features to keep are obtained via the chosen selecting method as
#'indicated by \code{select.by}.
#'
#'See the following functions for each specific implementation:
#'
#'\describe{
#' \item{\code{"cutoff"    }}{\code{\link{selectByCutoff}}}
#' \item{\code{"rank"      }}{\code{\link{selectByRanking}}}
#' \item{\code{"percentile"}}{\code{\link{selectByPercentile}}}
#' \item{\code{"fpr"       }}{\code{\link{selectByFpr}}}
#' \item{\code{"fdr"       }}{\code{\link{selectByFdr}}}
#'}
#'
#'@author Alessandro Barberis
#'
#'@seealso
#'Use \code{\link{listAvailableScreeningMethods}} to list the available
#'built-in screening methods.
#'
#'Use \code{\link{listAvailableSelectionFunctions}} to list the available
#'built-in selection functions.
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define row/col size
#'nr = 5
#'nc = 10
#'
#'# Unsupervised Screening
#'
#'#Data
#'x = matrix(
#'  data = sample(x = c(1,2), size = nr*nc, replace = TRUE),
#'  nrow = nr,
#'  ncol = nc,
#'  dimnames = list(
#'    paste0("f",seq(nr)),
#'    paste0("S",seq(nc))
#'  )
#')
#'
#'#Grouping variable
#'g = c(rep("a", nc/2), rep("b", nc/2))
#'
#'#Force 1st feature to have 40% of missing values
#'x[1,seq(nc*0.4)] = NA
#'
#'#Filter a feature if has more than 50% of missing values
#'screen(
#'  x = x,
#'  method = "missing.value",
#'  select.args = list(cutoff = 0.5)
#')
#'
#'# Supervised Screening
#'
#'#Filter by two-sample t-Test (cutoff on t statistic)
#'screen(
#'  x = x,
#'  g = g,
#'  method = "t.test",
#'  var = "equal",
#'  select.args = list(cutoff = 0.5)
#')
#'
#'@export
screen <- function(
    x,
    y         = NULL,
    g         = NULL,
    method    = c(
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
    ...,

    # Multi response
    multi = c("max", "min", "avg", "sum", "idx"),
    idx = NULL,

    # Selection
    select.by = c("cutoff", "rank", "percentile", "fpr", "fdr"),
    select.args = NULL
){

  # Setup ---------------------------------------------------------------------#
  method    = match.arg(method)
  multi     = match.arg(multi)
  select.by = match.arg(select.by)

  # Statistic -----------------------------------------------------------------#
  # Get statistical function
  statFun = getStatFunction(id = method)

  # Check whether g or y should be passed
  stat.args = list(x = x, y = y, g = g)
  stat.args = stat.args[intersect(names(stat.args), names(formals(fun = statFun)))]

  # Compute screening metric and significance
  metric = do.call(what = statFun, args = c(stat.args, list(...)))

  # Compute ranks
  ranks = defaultRanking(x = metric, id = method, multi = ifelse(test = identical(multi, 'idx'), yes = idx, no = multi))

  # If metric is a list with statistic/significance elements, pick up one to match
  # selection method
  if(isTRUE(is.list(metric))){
    screening.text = attr(metric, 'comment')
    if(isTRUE(select.by %in% c("fpr", "fdr"))){
      metric = metric$significance
    } else {
      metric = metric$statistic
    }
    attr(metric, 'comment') = screening.text
  }

  # Multi-response ------------------------------------------------------------#
  # Combine
  metric = multiresponse(x = metric, multi = multi, idx = idx)

  # Selection -----------------------------------------------------------------#
  # Get selection function
  selectFun = getSelectionFunction(id = select.by)

  # Match formals
  select.args = c(list(x = metric), select.args)
  select.args = select.args[intersect(names(select.args), names(formals(fun = selectFun)))]

  # Test which feature should be kept
  out = do.call(what = selectFun, args = select.args)

  # Output --------------------------------------------------------------------#

  # Create informative summary
  text = combineSummaryText(
    screening.text = attr(metric, 'comment'),
    screening.method = method,
    selection.text = attr(out, 'comment'),
    selection.method = select.by
  )

  out = featscreen(
    method    = method,
    multi     = multi,
    selection = select.by,
    summary   = text,
    n         = nrow(x),
    features  = rownames(x),
    keep      = out,
    ranks     = ranks
  )
  return(out)
}

