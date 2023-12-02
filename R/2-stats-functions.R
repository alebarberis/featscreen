#'@include 0-utility-functions.R 1-log-functions.R
NULL

# Vector -----------------------------------------------------------------------

#'Standard Deviation
#'
#'@description This function computes the *standard deviation* (SD) of the values in
#'\code{x}.
#'
#'See the **Details** section below for further information.
#'
#'@param x numerical vector.
#'@param g (optional) vector or factor object giving the group for the corresponding
#'elements of \code{x}.
#'@param na.rm logical indicating whether missing values should be removed before
#'computation.
#'
#'@details This function is a wrapper to the \code{\link[stats]{sd}} function.
#'
#'If \code{x} can be partitioned into \eqn{c} subgroups (provided by \code{g}),
#'then the \eqn{SD} is computed for each class.
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define size
#'n = 10
#'
#'#Data
#'x = sample.int(n = 100, size = n, replace = TRUE)
#'
#'#Grouping variable
#'g = c(rep("a", n/2), rep("b", n/2))
#'
#'#Standard deviation
#'sd(x)
#'
#'#Standard deviation by group
#'sd(x = x, g = g)
#'
#'@export
sd <- function(
    x,
    g = NULL,
    na.rm = TRUE
){

  if(isTRUE(is.null(g))){
    # Standard Deviation
    out = stats::sd(x = x, na.rm = na.rm)
  } else {
    # Standard Deviation by Group
    out = tapply(X = x, INDEX = g, FUN = stats::sd, na.rm = na.rm)
  }

  return(out)
}

#'Interquartile Range
#'
#'@description This function computes the *interquartile range* (IQR) of the values in
#'\code{x}.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams sd
#'
#'@details This function is a wrapper to the \code{\link[stats]{IQR}} function.
#'
#'If \code{x} can be partitioned into \eqn{c} subgroups (provided by \code{g}),
#'then the \eqn{IQR} is computed for each class.
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define size
#'n = 10
#'
#'#Data
#'x = sample.int(n = 100, size = n, replace = TRUE)
#'
#'#Grouping variable
#'g = c(rep("a", n/2), rep("b", n/2))
#'
#'#Interquartile Range
#'iqr(x)
#'
#'#Interquartile Range by group
#'iqr(x = x, g = g)
#'
#'@export
iqr <- function(
    x,
    g = NULL,
    na.rm = TRUE
){

  if(isTRUE(is.null(g))){
    # Interquartile Range
    out = stats::IQR(x = x, na.rm = na.rm)
  } else {
    # Interquartile Range by Group
    out = tapply(X = x, INDEX = g, FUN = stats::IQR, na.rm = na.rm)
  }

  return(out)
}

#'Median Absolute Deviation
#'
#'@description This function computes the *median absolute deviation* (MAD) of the values in
#'\code{x}.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams sd
#'
#'@details This function is a wrapper to the \code{\link[stats]{mad}} function.
#'
#'If \code{x} can be partitioned into \eqn{c} subgroups (provided by \code{g}),
#'then the \eqn{MAD} is computed for each class.
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define size
#'n = 10
#'
#'#Data
#'x = sample.int(n = 100, size = n, replace = TRUE)
#'
#'#Grouping variable
#'g = c(rep("a", n/2), rep("b", n/2))
#'
#'#Median Absolute Deviation
#'mad(x)
#'
#'#Median Absolute Deviation by group
#'mad(x = x, g = g)
#'
#'@export
mad <- function(
    x,
    g = NULL,
    na.rm = TRUE
){

  if(isTRUE(is.null(g))){
    # Interquartile Range
    out = stats::mad(x = x, na.rm = na.rm)
  } else {
    # Interquartile Range by Group
    out = tapply(X = x, INDEX = g, FUN = stats::mad, na.rm = na.rm)
  }

  return(out)
}

#'Relative Standard Deviation (Coefficient of Variation)
#'
#'@description This function computes the *coefficient of variation*
#'of the values in \code{x}. It is also known as *relative standard deviation*.
#'
#'See the **Details** section below for further information.
#'
#'@param x numerical vector.
#'@param g (optional) vector or factor object giving the group for the corresponding
#'elements of \code{x}.
#'@param na.rm logical indicating whether missing values should be removed before
#'computation.
#'
#'@return A numerical value or a vector containing the computed measure per class.
#'
#'@details The *coefficient of variation* - also known as *Normalized*
#'*Root-Mean-Square Deviation* (*NRMSD*), *Percent RMS*, and *relative standard*
#'*deviation* (*RSD*) - is a measure of statistical dispersion.
#'
#'It is defined as the ratio of the standard deviation to the mean:
#'
#'\deqn{ CV = RSD = \frac{standard deviation}{mean} = \frac{\sigma}{\mu}}
#'
#'It is be computed as:
#'
#'\deqn{ RSD = \frac{ \sqrt{\sum_{j=1}^{n}} ({x}_{j} - \bar{x})^2 / n }{ \bar{x} } }
#'
#'where \eqn{\bar{x}} is the sample mean.
#'
#'If \code{x} can be partitioned into \eqn{c} subgroups (provided by \code{g}),
#'then the \eqn{RSD} is computed for each class.
#'
#'The coefficient of variation ranges between \eqn{[-\infty, \infty]}.
#'
#'The main advantage of the coefficient of variation is that it is unitless.
#'
#'@author Alessandro Barberis
#'
#'@references
#'Fong, Biuk-Aghai, Si, Lightweight Feature Selection Methods Based on
#'Standardized Measure of Dispersion for Mining Big Data,
#'2016 IEEE International Conference on Computer and Information Technology (CIT)
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define size
#'n = 10
#'
#'#Data
#'x = sample.int(n = 100, size = n, replace = TRUE)
#'
#'#Grouping variable
#'g = c(rep("a", n/2), rep("b", n/2))
#'
#'#Coefficient of variation
#'rsd(x)
#'
#'#Coefficient of variation by group
#'rsd(x = x, g = g)
#'
#'@export
rsd <- function(
    x,
    g = NULL,
    na.rm = TRUE
  ){

  if(isTRUE(is.null(g))){
    # Standard Deviation
    stdev = stats::sd(x = x, na.rm = na.rm)
    # Mean
    m = mean(x = x, na.rm = na.rm)
  } else {
    # Standard Deviation by Group
    stdev = tapply(X = x, INDEX = g, FUN = stats::sd, na.rm = na.rm)
    # Mean by Group
    m = tapply(X = x, INDEX = g, FUN = mean, na.rm = na.rm)
  }

  # RSD
  rsd = stdev / m

  return(rsd)
}


#'Signal-to-noise Ratio
#'
#'@description This function computes the *signal-to-noise ratio*
#'of the values in \code{x}. It is the reciprocal of the coefficient of variation.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rsd
#'
#'@inherit rsd return
#'
#'@details The *signal-to-noise ratio* is here defined as the ratio of the mean to
#'the standard deviation:
#'
#'\deqn{ SNR = \frac{mean}{standard deviation} = \frac{\mu}{\sigma} }
#'
#'If \code{x} can be partitioned into \eqn{c} subgroups (provided by \code{g}),
#'then the \eqn{SNR} is computed for each class.
#'
#'@inherit rsd author
#'
#'@references https://en.wikipedia.org/wiki/Coefficient_of_variation
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define size
#'n = 10
#'
#'#Data
#'x = sample.int(n = 100, size = n, replace = TRUE)
#'
#'#Grouping variable
#'g = c(rep("a", n/2), rep("b", n/2))
#'
#'#Signal-to-noise ratio
#'snr(x)
#'
#'#Signal-to-noise ratio by group
#'snr(x = x, g = g)
#'
#'@export
snr <- function(
    x,
    g = NULL,
    na.rm = TRUE
){

  if(isTRUE(is.null(g))){
    # Standard Deviation
    stdev = stats::sd(x = x, na.rm = na.rm)
    # Mean
    m = mean(x = x, na.rm = na.rm)
  } else {
    # Standard Deviation by Group
    stdev = tapply(X = x, INDEX = g, FUN = stats::sd, na.rm = na.rm)
    # Mean by Group
    m = tapply(X = x, INDEX = g, FUN = mean, na.rm = na.rm)
  }

  # Signal-To-Noise Ratio
  out = m / stdev

  return(out)
}

#'Efficiency
#'
#'@description This function computes the efficiency as the square
#'of the coefficient of variation.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rsd
#'
#'@inherit rsd return
#'
#'@details The *efficiency* is a measure of dispersion. It is here defined as
#'the square ratio of the standard deviation to the mean:
#'
#'\deqn{ EFFICIENCY = (\frac{standard deviation}{mean})^2 = \frac{\sigma^2}{\mu^2}}
#'
#'If \code{x} can be partitioned into \eqn{c} subgroups (provided by \code{g}),
#'then the \eqn{EFFICIENCY} is computed for each class.
#'
#'@inherit rsd author
#'
#'@references https://en.wikipedia.org/wiki/Efficiency_(statistics)#Estimators_of_u.i.d._Variables
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define size
#'n = 10
#'
#'#Data
#'x = sample.int(n = 100, size = n, replace = TRUE)
#'
#'#Grouping variable
#'g = c(rep("a", n/2), rep("b", n/2))
#'
#'#Efficiency
#'efficiency(x)
#'
#'#Efficiency by group
#'efficiency(x = x, g = g)
#'
#'@export
efficiency <- function(
    x,
    g = NULL,
    na.rm = TRUE
){

  if(isTRUE(is.null(g))){
    # Standard Deviation
    stdev = stats::sd(x = x, na.rm = na.rm)
    # Mean
    m = mean(x = x, na.rm = na.rm)
  } else {
    # Standard Deviation by Group
    stdev = tapply(X = x, INDEX = g, FUN = stats::sd, na.rm = na.rm)
    # Mean by Group
    m = tapply(X = x, INDEX = g, FUN = mean, na.rm = na.rm)
  }

  # Efficiency
  out = (stdev / m)^2

  return(out)
}


#'Variance-to-mean Ratio
#'
#'@description This function computes the *variance-to-mean ratio*.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rsd
#'
#'@inherit rsd return
#'
#'@details The *variance-to-mean ratio* (also known as *index of dispersion*)
#'is a measure of dispersion. It is computed as:
#'
#'\deqn{ VMR = \frac{variance}{mean} = \frac{\sigma^2}{\mu}}
#'
#'If \code{x} can be partitioned into \eqn{c} subgroups (provided by \code{g}),
#'then the \eqn{VMR} is computed for each class.
#'
#'@inherit rsd author
#'
#'@references https://en.wikipedia.org/wiki/Index_of_dispersion
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Define size
#'n = 10
#'
#'#Data
#'x = sample.int(n = 100, size = n, replace = TRUE)
#'
#'#Grouping variable
#'g = c(rep("a", n/2), rep("b", n/2))
#'
#'#Variance-to-mean ratio
#'vmr(x)
#'
#'#Variance-to-mean ratio by group
#'vmr(x = x, g = g)
#'
#'@export
vmr <- function(
    x,
    g = NULL,
    na.rm = TRUE
){

  if(isTRUE(is.null(g))){
    # Variance
    variance = stats::var(x = x, na.rm = na.rm)
    # Mean
    m = mean(x = x, na.rm = na.rm)
  } else {
    # Variance by Group
    variance = tapply(X = x, INDEX = g, FUN = stats::var, na.rm = na.rm)
    # Mean by Group
    m = tapply(X = x, INDEX = g, FUN = mean, na.rm = na.rm)
  }

  # Variance-to-mean ratio
  out = variance / m

  return(out)
}

# Row Functions-----------------------------------------------------------------

#'Missing Value Ratio
#'
#'@description This function computes the ratio of missing elements.
#'
#'See the **Details** section below for further information.
#'
#'@param x \code{matrix} or \code{data.frame}, where rows are features and columns are observations.
#'@param g (optional) vector or factor object giving the group for the corresponding
#'elements of \code{x}.
#'
#'@return A \code{vector} of length \code{nrow(x)} containing the computed ratios.
#'If \code{g} is provided, a \code{matrix} with ratios for each class as column
#'vectors is returned.
#'
#'@details If \code{g = NULL}, for each feature a missing value ratio (MVR) is computed as:
#'
#'\deqn{Missing Value Ratio (MVR) = \frac{Number of missing values}{Total number of observations}}
#'
#'If \code{g} is provided, the missing value ratios \eqn{MVR_{ij}} are computed
#'for each group \eqn{j} as:
#'
#'\deqn{MVR_{ij} = \frac{Number of missing values in j-th class}{Number of observations in j-th class}}
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
#'#Compute MVR
#'rowMissingValueRatio(x = x)
#'
#'#Compute MVR by class
#'rowMissingValueRatio(x = x, g = g)
#'
#'@export
rowMissingValueRatio <- function(
    x,
    g = NULL
  ){

  # Setup
  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (is.null(g) || (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x)))))

  #Filter

  #Test which value is NA
  test = is.na(x)

  if(isTRUE(is.null(g))){
    # Compute number of times features have missing values
    row.sums = rowSums(x = test, na.rm = TRUE)
    # Compute ratios
    ratios = row.sums / ncol(x)
  } else {
    # Compute number of times features have missing values by group
    row.sums = t(apply(X = test, MARGIN = 1, FUN = tapply, INDEX = g, sum, na.rm = TRUE))
    # Compute ratios
    ratios = row.sums / as.vector(table(g)[colnames(row.sums)])
  }

  # Describe
  attr(x = ratios, which = "comment") = getStatisticalMethodName(id = "missing.value")

  # End
  return(ratios)
}


#'Above-Median Frequency Ratio
#'
#'@description This function computes the ratio of values above the median for
#'each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rowMissingValueRatio
#'
#'@inherit rowMissingValueRatio return
#'
#'@details For each observation, the median is computed via
#'\code{\link[matrixStats]{colMedians}}.
#'
#'Remember that the median across \eqn{n} elements is defined as:
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
#'If \code{g} is provided, the above-median frequency ratio (\eqn{AMFR_{ij}}) is
#'computed for each group \eqn{j} as:
#'
#'\deqn{AMFR_{ij} = \frac{Number of samples in j-th class where feature is above the sample median}{Number of observations in j-th class}}
#'
#'@inherit rowMissingValueRatio author
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
#'#AMR
#'rowAboveMedianFreqRatio(x)
#'
#'#AMR by group
#'rowAboveMedianFreqRatio(x = x, g = g)
#'
#'@export
rowAboveMedianFreqRatio <- function(
    x,
    g = NULL
){
  # Setup
  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (is.null(g) || (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x)))))

  # Filter

  #Compute median across features for each observation
  col.medians = matrixStats::colMedians(x = x, na.rm = TRUE)

  # Test if features expression is above median (features are returned by columns)
  test = apply(X = x, MARGIN = 1, FUN = ">=", col.medians)

  if(isTRUE(is.null(g))){
    # Compute number of times feature expression is above sample median
    row.sums = colSums(x = test, na.rm = TRUE)
    # Compute ratios
    ratios = row.sums / ncol(x)
  } else {
    # Compute number of times feature expression is above sample median by group
    row.sums = t(apply(X = test, MARGIN = 2, FUN = tapply, INDEX = g, sum, na.rm = TRUE))
    # Compute ratios
    ratios = row.sums / as.vector(table(g)[colnames(row.sums)])
  }

  # Describe
  attr(x = ratios, which = "comment") = getStatisticalMethodName(id = "above.median")

  # End
  return(ratios)
}


#'Above-Minimum Frequency Ratio
#'
#'@description This function computes the ratio of values above a minimum value for
#'each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rowMissingValueRatio
#'@param min.expr numerical value indicating the minimum expression required for
#'\code{min.prop} samples.
#'
#'@inherit rowMissingValueRatio return
#'
#'@details For each feature, the above-minimum frequency ratio (AMFR) is computed as:
#'
#'\deqn{Above-Minimum Frequency Ratio (AMFR) = \frac{Number of samples where expression is greater than provided minimum}{Total number of observations}}
#'
#'If \code{g} is provided, the above-minimum frequency ratio (\eqn{AMFR_{ij}}) is
#'computed for each group \eqn{j} as:
#'
#'\deqn{AMFR_{ij} = \frac{Number of samples in j-th class where expression is greater than provided minimum}{Total number of observations in j-th class}}
#'
#'
#'@inherit rowMissingValueRatio author
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
#'#AMFR
#'rowAboveMinFreqRatio(x)
#'
#'#AMFR by group
#'rowAboveMinFreqRatio(x = x, g = g)
#'
#'@export
rowAboveMinFreqRatio <- function(
    x,
    g = NULL,
    min.expr = 0
){

  # Setup
  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (is.null(g) || (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x)))))

  # Filter

  # Test if features expression is above minimum value
  test = x >= min.expr

  if(isTRUE(is.null(g))){
    # Compute number of times expression is above minimum value
    row.sums = rowSums(x = test, na.rm = TRUE)
    # Compute ratios
    ratios = row.sums / ncol(x)
  } else {
    # Compute number of times expression is above minimum value by group
    row.sums = t(apply(X = test, MARGIN = 1, FUN = tapply, INDEX = g, sum, na.rm = TRUE))
    # Compute ratios
    ratios = row.sums / as.vector(table(g)[colnames(row.sums)])
  }

  # Describe
  attr(x = ratios, which = "comment") = getStatisticalMethodName(id = "above.minimum")

  # End
  return(ratios)
}


#'Variability
#'
#'@description This function computes measures of variability.
#'
#'@inheritParams rowMissingValueRatio
#'
#'@param method character string indicating the measure of variability.
#'Available options are:
#'\describe{
#'   \item{\code{"sd"}}{the standard deviation}
#'   \item{\code{"iqr"}}{the interquartile range}
#'   \item{\code{"mad"}}{the median absolute deviation}
#'   \item{\code{"rsd"}}{the relative standard deviation (i.e., coefficient of variation)}
#'   \item{\code{"efficiency"}}{the coefficient of variation squared}
#'   \item{\code{"vmr"}}{the variance-to-mean ratio}
#'}
#'
#'@inherit rowMissingValueRatio return
#'
#'@details
#'
#'The corrected sample *standard deviation* is the defined as:
#'
#'\deqn{ SD = \sqrt{ \frac{1}{N-1} \sum_{i=1}{N}(x_{i} - \bar{x})^2 } }
#'
#'where \eqn{x} is a vector of \eqn{N} elements, and \eqn{\bar{x}} is its mean value.
#'
#'The *interquartile range* is defined as the difference between the 75th and
#'25th percentiles of the data:
#'
#'\deqn{ IQR = Q_{3} - Q_{1})}
#'
#'The *median absolute deviation* is defined as the median of the absolute
#'deviations from the mean:
#'
#'\deqn{ MAD = \median(|x_{i} - \bar{x}|)}
#'
#'The *relative standard variation* is defined as the ratio of the standard deviation
#'to the mean:
#'
#'\deqn{ RSD = \frac{standard deviation}{mean} = \frac{\sigma}{\mu}}
#'
#'The *efficiency* is the square of the coefficient of variation:
#'
#'\deqn{ efficiency = (\frac{standard deviation}{mean})^2 = \frac{\sigma^2}{\mu^2}}
#'
#'The *variance-to-mean ratio* is computed as the ratio of the variance to the mean:
#'
#'\deqn{ VMR = \frac{variance}{mean} = \frac{\sigma^2}{\mu}}
#'
#'See also the following functions for further details:
#'\describe{
#' \item{\code{"sd"        }}{\code{\link{sd}}}
#' \item{\code{"iqr"       }}{\code{\link{iqr}}}
#' \item{\code{"mad"       }}{\code{\link{mad}}}
#' \item{\code{"rsd"       }}{\code{\link{rsd}}}
#' \item{\code{"efficiency"}}{\code{\link{efficiency}}}
#' \item{\code{"vmr"       }}{\code{\link{vmr}}}
#'}
#'
#'@inherit rowMissingValueRatio author
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
#'#Variance-to-mean Ratio
#'rowVariability(x = x, method = 'vmr')
#'
#'#Variance-to-mean Ratio by group
#'rowVariability(x = x, g = g, method = 'vmr')
#'
#'@export
rowVariability <- function(
    x,
    g = NULL,
    method = c("sd", "iqr", "mad", "rsd", "efficiency", "vmr")
){

  # Setup
  method = match.arg(method)

  # Filter

  # Compute variability
  out = switch(
    method,
    'sd'         = apply(X = x, MARGIN = 1, FUN = sd        , g = g, na.rm = TRUE),
    'iqr'        = apply(X = x, MARGIN = 1, FUN = iqr       , g = g, na.rm = TRUE),
    'mad'        = apply(X = x, MARGIN = 1, FUN = mad       , g = g, na.rm = TRUE),
    'rsd'        = apply(X = x, MARGIN = 1, FUN = rsd       , g = g, na.rm = TRUE),
    'vmr'        = apply(X = x, MARGIN = 1, FUN = vmr       , g = g, na.rm = TRUE),
    'efficiency' = apply(X = x, MARGIN = 1, FUN = efficiency, g = g, na.rm = TRUE)
  );

  # Transpose
  if(isFALSE(is.null(dim(out)))){
    out = t(out)
  }

  # Describe
  attr(x = out, which = "comment") = paste(
    "variability measured as",
    getVariabilityMeasureName(method)
  )

  # End
  return(out)
}


#'Medians
#'
#'@description This function computes the median value for
#'each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rowMissingValueRatio
#'
#'@inherit rowMissingValueRatio return
#'
#'@details If \code{g = NULL}, for each feature the median is computed via
#'\code{\link[matrixStats]{rowMedians}}.
#'
#'If \code{g} is provided, the median per group is computed via
#'\code{\link[stats]{median}}.
#'
#'@inherit rowMissingValueRatio author
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
#'#Medians
#'rowMedians(x)
#'
#'#Medians by group
#'rowMedians(x = x, g = g)
#'
#'@export
rowMedians <- function(
    x,
    g = NULL
  ){
  # Setup
  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (is.null(g) || (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x)))))

  # Filter
  if(isTRUE(is.null(g))){
    # For each feature compute median across observations
    out = matrixStats::rowMedians(x = x, na.rm = TRUE)
  } else {
    # For each feature compute median across observations by group
    out = t(apply(X = x, MARGIN = 1, FUN = tapply, INDEX = g, stats::median, na.rm = TRUE))
  }

  # Describe
  attr(x = out, which = "comment") = paste(
    "median value"
  )

  # End
  return(out)
}
