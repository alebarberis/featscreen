#'@include 0-utility-functions.R 1-log-functions.R
NULL

# Univariate -------------------------------------------------------------------

#'Select by Cutoff
#'
#'@description Select elements based on a cutoff on their values.
#'
#'@param x numerical vector.
#'@param cutoff numeric value indicating the cutoff.
#'@param operator character string indicating the relational operator to use.
#'
#'@return A logical vector, indicating the elements to keep.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = seq(10)
#'names(x) = paste0("f", seq(10))
#'
#'#Select
#'selectByCutoff(x = x, cutoff = 5, operator = "<")
#'
#'@export
selectByCutoff <- function(
    x,
    cutoff,
    operator = c("<", "<=", ">", ">=")
){
  # Check
  stopifnot("'cutoff' must be a numerical value. Please, check your input.\n" =
              (!missing(cutoff) && is.numeric(cutoff)))
  # Default
  out = stats::setNames(
    object = vector(mode = "logical", length = length(x)),
    nm = names(x)
  )
  # Match
  operator = match.arg(operator)
  # Test
  keep = !is.na(x) & do.call(what = operator, args = list(x = x, y = cutoff))
  # Select
  out[keep] = TRUE
  # Describe
  attr(x = out, which = "comment") = paste(
    sum(out), "out of", length(x),
    "features selected by a cutoff",
    paste0("(",operator," ",cutoff,")")
  )
  # Return
  return(out)
}

#'Select by Ranking
#'
#'@description Select elements based on their ranking.
#'
#'@param x numerical vector.
#'@param k integer value indicating the number of top elements to keep.
#'@param method character string indicating the method used for sorting (see
#'\code{\link[base]{sort}}).
#'@param decreasing logical indicating whether to sort in decreasing order.
#'@param use.abs logical indicating whether to use absolute values.
#'
#'@return A logical vector indicating the elements to keep.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = seq(10)
#'names(x) = paste0("f", seq(10))
#'
#'#Select
#'selectByRanking(x = x, k = 5)
#'
#'@export
selectByRanking <- function(
    x,
    k = length(x),
    method = c("auto", "shell", "quick", "radix"),
    decreasing = TRUE,
    use.abs = FALSE
){
  # Coerce
  k = as.integer(k)
  # Check
  stopifnot("'k' must be a positive integer value at most equal to length(x). Please, check your input.\n" =
              (k>0 && k<=length(x)))
  stopifnot("'decreasing' must be a logical value" = is.logical(decreasing))
  stopifnot("'use.abs' must be a logical value" = is.logical(use.abs))

  # Default
  out = stats::setNames(
    object = vector(mode = "logical", length = length(x)),
    nm = names(x)
  )
  # Match
  method = match.arg(method)
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
    method       = method
  )$ix
  # Select
  out[index[seq_len(k)]] = TRUE
  # Describe
  attr(x = out, which = "comment") = paste(
    sum(out), "out of", length(x),
    "features selected according to their",
    ifelse(test = decreasing, yes = "descending", no = "ascending"),
    "ranking"
  )
  # Return
  return(out)
}

#'Select by Percentile
#'
#'@description Select elements based on the percentile of the highest values.
#'
#'@param x numerical vector.
#'@param percentile numerical value in the range \eqn{[0, 1]} indicating the
#'percentage of elements to keep.
#'
#'@return A logical vector, indicating the elements to keep.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = seq(10)
#'names(x) = paste0("f", seq(10))
#'
#'#Select
#'selectByPercentile(x = x, percentile = 0.25)
#'
#'@export
selectByPercentile <- function(x, percentile = 0.25){
  # Check
  stopifnot("'percentile' must be a numeric value in the range [0,1]. Please, check your input.\n" =
              is.numeric(percentile) && (percentile>=0 && percentile<=1))
  # Default
  out = stats::setNames(
    object = vector(mode = "logical", length = length(x)),
    nm = names(x)
  )
  # Compute threshold
  threshold = stats::quantile(x = x, probs=(1 - percentile), na.rm=TRUE)
  # Test which feature should be kept
  keep = !is.na(x) & x > threshold
  # Select
  out[keep] = TRUE
  # Test ties
  ties = which(!is.na(x) & x == threshold)
  # Check ties
  if(isTRUE(length(ties)>0)){
    maxfeats = trunc(length(x) * percentile)
    keep = ties[seq_len(maxfeats - sum(out))]
    # Update
    out[keep] = TRUE
  }
  # Describe
  attr(x = out, which = "comment") = paste(
    sum(out), "out of", length(x),
    "features selected according to a percentile",
    paste0("(top ",round(percentile*100, digits = 1),"%)")
  )
  # Return
  return(out)
}

#'Select by False Positive Rate (FPR)
#'
#'@description Select elements based on the false positive rate.
#'
#'@param x numerical vector indicating the p-values.
#'@param alpha numerical value, the upper-bound on the FPR.
#'@param operator character string indicating the relational operator to use.
#'
#'@return A logical vector, indicating the elements to keep.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = seq(from = 0, to = 1, by = 0.1)
#'names(x) = paste0("f", seq(11))
#'
#'#Select
#'selectByFpr(x = x, alpha = 0.3)
#'
#'@export
selectByFpr <- function(x, alpha = 0.05, operator = c("<", "<=")){
  # Check
  stopifnot("'alpha' must be a numeric value in the range [0,1]. Please, check your input.\n" =
              is.numeric(alpha) && (alpha>=0 && alpha<=1))
  # Match
  operator = match.arg(operator)
  # Default
  out = stats::setNames(
    object = vector(mode = "logical", length = length(x)),
    nm = names(x)
  )
  # Test
  keep = !is.na(x) & do.call(what = operator, args = list(x = x, y = alpha))
  # Select
  out[keep] = TRUE
  # Describe
  attr(x = out, which = "comment") = paste(
    sum(out), "out of", length(x),
    "features selected according to a cutoff on the false positive rate",
    paste0("(p-value ", operator, " ", round(alpha, digits = 3),")")
  )
  # Return
  return(out)
}

#'Select by False Discovery Rate
#'
#'@description Select elements based on the false discovery rate. The provided
#'significance (\code{x}) is corrected via the Benjamini-Hochberg procedure.
#'
#'@param x numerical vector indicating the p-values.
#'@param alpha numerical value, the upper-bound on the FDR.
#'@param operator character string indicating the relational operator to use.
#'
#'@return A logical vector, indicating the elements to keep.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = seq(from = 0, to = 1, by = 0.1)
#'names(x) = paste0("f", seq(11))
#'
#'#Select
#'selectByFdr(x = x, alpha = 0.3)
#'
#'@export
selectByFdr <- function(x, alpha = 0.05, operator = c("<", "<=")){
  # Check
  stopifnot("'alpha' must be a numeric value in the range [0,1]. Please, check your input.\n" =
              is.numeric(alpha) && (alpha>=0 && alpha<=1))
  # Match
  operator = match.arg(operator)
  # Default
  out = stats::setNames(
    object = vector(mode = "logical", length = length(x)),
    nm = names(x)
  )
  # Adjust
  x = stats::p.adjust(p = x, method = "fdr")
  # Test
  keep = !is.na(x) & do.call(what = operator, args = list(x = x, y = alpha))
  # Select
  out[keep] = TRUE
  # Describe
  attr(x = out, which = "comment") = paste(
    sum(out), "out of", length(x),
    "features selected according to a cutoff on the false discovery rate",
    paste0("(fdr ", operator, " ", round(alpha, digits = 3),")")
  )
  # Return
  return(out)
}

# Get Selecting Functions ------------------------------------------------------

#'Get Selection Function
#'
#'@description This function is a dispatcher for the selecting function
#'in input.
#'
#'@param id character string, one of the supported selecting techniques.
#'
#'@return A selecting function:
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
#'@examples
#'f = getSelectionFunction(id = 'cutoff')
#'
#'@export
getSelectionFunction <- function(
    id = c("cutoff", "rank", "percentile", "fpr", "fdr")
){

  #match arg
  id = match.arg(id)

  #get function
  f = switch(
    id,
    "cutoff"     = selectByCutoff,
    "rank"       = selectByRanking,
    "percentile" = selectByPercentile,
    "fpr"        = selectByFpr,
    "fdr"        = selectByFdr
  )

  #return
  return(f)
}
