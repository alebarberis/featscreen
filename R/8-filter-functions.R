#'@include 0-utility-functions.R 1-log-functions.R 2-stats-functions.R
NULL

# Filters ----------------------------------------------------------------------

#'Filter by Missing Value Ratio
#'
#'@description This function filters the input matrix \code{x} depending on the
#'presence of missing values.
#'A variable is removed if the ratio of missing elements is greater than a given
#'percentage defined by \code{max.prop}.
#'
#'See the **Details** section below for further information.
#'
#'@param x \code{matrix} or \code{data.frame}, where rows are features and columns are observations.
#'@param g (optional) vector or factor object giving the group for the corresponding
#'elements of \code{x}.
#'@param max.prop numerical value in the range \eqn{[0, 1]}. Maximum proportion of
#'samples with missing values. Default to \code{0.5}.
#'
#'@return A logical vector of length \code{nrow(x)} indicating which rows of
#'\code{x} passed the filter.
#'
#'@details If \code{g = NULL}, for each feature a missing value ratio (MVR) is computed as:
#'
#'\deqn{Missing Value Ratio (MVR) = \frac{Number of missing values}{Total number of observations}}
#'
#'Then, the i-th feature is kept if \eqn{MVR_{i} < max.prop}.
#'
#'If \code{g} is provided, the missing value ratios \eqn{MVR_{ij}} are computed
#'for each group \eqn{j}.
#'
#'Then, the i-th feature is kept if \eqn{MVR_{ij} < max.prop} for each group.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define row/col size
#'nr = 5
#'nc = 10
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
#'rowFilterByMissingValueRatio(x = x, max.prop = 0.5)
#'
#'#Filter a feature if has more than 30% of missing values
#'rowFilterByMissingValueRatio(x = x, max.prop = 0.3)
#'
#'#Set 3rd feature to have 40% of missing values for each class
#'x[3,seq(nc*0.4)] = NA
#'x[3,(seq(nc*0.4)+nc/2)] = NA
#'
#'#Filter a feature if has more than 50% of missing values
#'rowFilterByMissingValueRatio(x = x, max.prop = 0.5)
#'
#'#Filter a feature if has more than 50% of missing values in any group
#'rowFilterByMissingValueRatio(x = x, max.prop = 0.5, g = g)
#'
#'@export
rowFilterByMissingValueRatio <- function(
    x,
    g = NULL,
    max.prop = 0.5
  ){

  # Setup
  stopifnot("'max.prop' must be a numeric value in the range [0, 1]. Please, check your input.\n" =
              is.numeric(max.prop) && (max.prop >= 0) && (max.prop <= 1))

  #Filter

  # Compute ratios
  ratios = rowMissingValueRatio(x = x, g = g)

  # Check multi
  ratios = multiresponse(x = ratios, multi = "max")

  # Test which feature should be kept
  keep = selectByCutoff(x = ratios, cutoff = max.prop, operator = '<')

  # Update
  attr(x = keep, which = 'comment') = paste(
    attr(x = keep, which = 'comment'),
    "on the missing value ratio"
  )

  # End
  return(keep)
}


#'Filter by Above-Median Frequency Ratio
#'
#'@description This function filters the input matrix \code{x} depending on the
#'features' intensity.
#'Variables are removed if their values are lower than the medians computed
#'across features in a given percentage of samples defined by \code{min.prop}.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rowFilterByMissingValueRatio
#'@param min.prop numerical value in the range \eqn{[0, 1]}. Minimum proportion of
#'samples where the feature expression is above the median. Default to \code{0.5}.
#'
#'@inherit rowFilterByMissingValueRatio return
#'
#'@details For each observation, the median across \eqn{n} features is computed as:
#'
#'\deqn{Median = x_\frac{(n+1)}{2}}
#'
#'where \eqn{x} is an ascendingly ordered vector of \eqn{n} elements, and \eqn{n}
#'is odd. If \eqn{n} is even, then the median is computed as:
#'
#'\deqn{Median = \frac{x_\frac{n}{2}+x_{\frac{(n)}{2}+1}}{2}}
#'
#'If \code{g = NULL}, for each feature we define an above-median frequency ratio (AMFR) as the
#'number of times the feature value is greater than the sample median divided
#'by the total number of observations:
#'
#'\deqn{Above-Median Frequency Ratio (AMFR) = \frac{Number of samples where feature is above the sample median}{Total number of observations}}
#'
#'Finally, the i-th feature is kept if \eqn{AMFR_{i} >= min.prop}.
#'
#'If \code{g} is provided, the above-median frequency ratio (\eqn{AMFR_{ij}}) is
#'computed for each group \eqn{j}.
#'The i-th feature is kept if \eqn{AMFR_{ij} >= min.prop} for at least one group.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define row/col size
#'nr = 5
#'nc = 10
#'
#'#Data
#'x = matrix(
#'  data = sample.int(n = 100, size = nr*nc, replace = TRUE),
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
#'#Filter
#'rowFilterByAboveMedianRatio(x)
#'
#'#Filter by group
#'rowFilterByAboveMedianRatio(x = x, g = g)
#'
#'@export
rowFilterByAboveMedianRatio <- function(
    x,
    g = NULL,
    min.prop = 0.5
){
  # Setup
  stopifnot("'min.prop' must be a numeric value in the range [0, 1]. Please, check your input.\n" =
              is.numeric(min.prop) && (min.prop >= 0) && (min.prop <= 1))
  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (is.null(g) || (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x)))))

  # Filter

  # Compute ratios
  ratios = rowAboveMedianFreqRatio(x = x, g = g)

  # Check multi
  ratios = multiresponse(x = ratios, multi = "max")

  # Test which feature should be kept
  keep = selectByCutoff(x = ratios, cutoff = min.prop, operator = ">=")

  # Update
  attr(x = keep, which = 'comment') = paste(
    attr(x = keep, which = 'comment'),
    "on the above-median frequency ratio"
  )

  # End
  return(keep)
}



#'Filter by Above-Minimum Frequency Ratio
#'
#'@description This function filters the input matrix \code{x} depending on the
#'features' intensity.
#'Variables are removed if their values are lower than a provided minimum value
#'in a given percentage of samples defined by \code{min.prop}.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rowFilterByMissingValueRatio
#'@param min.expr numerical value indicating the minimum expression required for
#'\code{min.prop} samples.
#'@param min.prop numerical value in the range \eqn{[0, 1]}. Minimum proportion of
#'samples where the feature expression should be above \code{min.expr}.
#'Default to \code{0.5}.
#'
#'@inherit rowFilterByMissingValueRatio return
#'
#'@details For each feature, the above-minimum frequency ratio (AMFR) is computed as:
#'
#'\deqn{Above-Minimum Frequency Ratio (AMFR) = \frac{Number of samples where expression is greater than provided minimum}{Total number of observations}}
#'
#'Then, the i-th feature is kept if \eqn{AMFR_{i} >= min.prop}.
#'
#'If \code{g} is provided, the above-minimum frequency ratio (\eqn{AMFR_{ij}}) is
#'computed for each group \eqn{j}.
#'The i-th feature is kept if \eqn{AMFR_{ij} >= min.prop} for at least one group.
#'
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define row/col size
#'nr = 5
#'nc = 10
#'
#'#Data
#'x = matrix(
#'  data = sample.int(n = 100, size = nr*nc, replace = TRUE),
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
#'#Filter
#'rowFilterByAboveMinRatio(x)
#'
#'#Filter by group
#'rowFilterByAboveMinRatio(x = x, g = g)
#'
#'#Set 1st feature to 0s for 2/3 observations
#'x[1,seq(2*nc/3)] = 0
#'
#'#Set 3rd feature to 0s for 2/3 observations of class "a" and "b"
#'x[3,seq(2*nc/6)] = 0
#'x[3,(seq(2*nc/6)+nc/2)] = 0
#'
#'#Filter (1st and 3rd features should be flagged to be removed)
#'rowFilterByAboveMinRatio(x = x, min.expr = 10)
#'
#'#Filter by group (3rd feature should be flagged to be removed)
#'rowFilterByAboveMinRatio(x = x, min.expr = 10, g = g)
#'
#'@export
rowFilterByAboveMinRatio <- function(
    x,
    g = NULL,
    min.expr = 0,
    min.prop = 0.5
){

  # Setup
  stopifnot("'min.prop' must be a numeric value in the range [0, 1]. Please, check your input.\n" =
              is.numeric(min.prop) && (min.prop >= 0) && (min.prop <= 1))
  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (is.null(g) || (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x)))))

  # Filter

  # Compute ratios
  ratios = rowAboveMinFreqRatio(x = x, g = g, min.expr = min.expr)

  # Check multi
  ratios = multiresponse(x = ratios, multi = "max")

  # Test which feature should be kept
  keep = selectByCutoff(x = ratios, cutoff = min.prop, operator = ">=")

  # Update
  attr(x = keep, which = 'comment') = paste(
    attr(x = keep, which = 'comment'),
    "on the above-minimum frequency ratio"
  )

  # End
  return(keep)
}



#'Filter by Median Above Minimum Expression
#'
#'@description This function filters the input matrix \code{x} depending on the
#'features' intensity.
#'Variables are removed if their median values are lower than a provided minimum.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rowFilterByMissingValueRatio
#'@param min.expr numerical value indicating the median minimum expression required.
#'
#'@inherit rowFilterByMissingValueRatio return
#'
#'@details If \code{g = NULL}, the median of each feature is computed across all
#'observations via \code{\link[matrixStats]{rowMedians}}.
#'Then, the i-th feature is kept if \eqn{median_{i} >= min.expr}.
#'
#'If \code{g} is provided, the median per group is computed for each feature via
#'\code{\link[stats]{median}}.
#'
#'Then, the i-th feature is kept if \eqn{median_{ig} >= min.expr} in at least
#'one group.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define row/col size
#'nr = 5
#'nc = 10
#'
#'#Data
#'x = matrix(
#'  data = sample.int(n = 100, size = nr*nc, replace = TRUE),
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
#'#Filter
#'rowFilterByMedianAboveMinExpr(x)
#'
#'#Filter by group
#'rowFilterByMedianAboveMinExpr(x = x, g = g)
#'
#'#Set 1st feature to 0s for 2/3 observations
#'x[1,seq(2*nc/3)] = 0
#'
#'#Set 2nd feature to 0s for 2/3 observations of class "a"
#'x[2,seq(2*nc/6)] = 0
#'
#'#Set 3rd feature to 0s for 2/3 observations of class "a" and "b"
#'x[3,seq(2*nc/6)] = 0
#'x[3,(seq(2*nc/6)+nc/2)] = 0
#'
#'#Filter (1st and 3rd features should be flagged to be removed)
#'rowFilterByMedianAboveMinExpr(x = x, min.expr = 1)
#'
#'
#'#Filter by group (3rd feature should be flagged to be removed)
#'rowFilterByMedianAboveMinExpr(x = x, g = g, min.expr = 1)
#'
#'@export
rowFilterByMedianAboveMinExpr <- function(
    x,
    g = NULL,
    min.expr = 0
){
  # Setup
  stopifnot("'min.expr' must be a numeric value. Please, check your input.\n" =
              is.numeric(min.expr))
  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (is.null(g) || (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x)))))

  # Compute medians
  row.medians = rowMedians(x = x, g = g)

  # Check multi
  row.medians = multiresponse(x = row.medians, multi = "max")

  # Test which feature should be kept
  keep = selectByCutoff(x = row.medians, cutoff = min.expr, operator = ">=")

  # End
  return(keep)
}

#'Filter by Low Variability
#'
#'@description This function filters the input matrix \code{x} depending on the
#'features' variability.
#'
#'See the \code{\link{rowVariability}} for further information.
#'
#'@inheritParams rowFilterByMissingValueRatio
#'@inheritParams rowVariability
#'@param percentile numerical value in the range \eqn{[0, 1]} indicating the
#'percentage of features to keep.
#'
#'@inherit rowFilterByMissingValueRatio return
#'@author Alessandro Barberis
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define row/col size
#'nr = 5
#'nc = 10
#'
#'#Data
#'x = matrix(
#'  data = sample.int(n = 100, size = nr*nc, replace = TRUE),
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
#'#Filter
#'rowFilterByLowVariability(x)
#'
#'#Filter by group
#'rowFilterByLowVariability(x = x, g = g)
#'
#'@export
rowFilterByLowVariability <- function(
    x,
    g = NULL,
    percentile = 0.25,
    method = c("sd", "iqr", "mad", "rsd", "vmr", "efficiency")
  ){

  # Setup
  method = match.arg(method)
  stopifnot("'percentile' must be a numeric value in the range [0,1]. Please, check your input.\n" =
              is.numeric(percentile) && (percentile>=0 && percentile<=1))
  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (is.null(g) || (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x)))))

  # Filter

  # Compute features variability
  metric = rowVariability(x = x, g = g, method = method);

  # Combine
  metric = multiresponse(x = metric, multi = "sum")

  # Test which feature should be kept
  keep = selectByPercentile(x = metric, percentile = percentile)

  # Update
  attr(x = keep, which = 'comment') = paste(
    attr(x = keep, which = 'comment'),
    "of the highest variable features as measured by",
    getVariabilityMeasureName(id = method)
  )

  # End
  return(keep)
}
