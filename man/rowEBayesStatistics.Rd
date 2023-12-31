% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/4-biostats-test-functions.R
\name{rowEBayesStatistics}
\alias{rowEBayesStatistics}
\title{Empirical Bayes Moderated Statistics}
\usage{
rowEBayesStatistics(
  x,
  y = NULL,
  observations = NULL,
  statistic = c("moderated.F", "moderated.t"),
  technology = c("array", "seq"),
  is.logged = TRUE,
  mean.variance = c("ebayes", "weights"),
  span = 0.5,
  method = c("ls", "robust"),
  design = NULL,
  weights = NULL,
  ndups = NULL,
  spacing = NULL,
  block = NULL,
  correlation = NULL,
  contrasts = NULL,
  proportion = 0.01,
  stdev.coef.lim = c(0.1, 4),
  robust = FALSE,
  winsor.tail.p = c(0.05, 0.1),
  coef = NULL,
  adjust.method = "BH",
  logger = NULL,
  ...
)
}
\arguments{
\item{x}{a matrix-like data object with rows corresponding to genes and
columns to observations.}

\item{y}{a vector, factor or matrix. It is used to create a design matrix if
not explicitly provided via the \code{design} argument.}

\item{observations}{(optional) integer vector, the indices of observations to
keep.}

\item{statistic}{character string, indicating the moderated statistic to return.}

\item{technology}{character string, the technology used to generate the data.
Available options are:
\describe{
\item{array}{data generated with microarray technology}
\item{seq}{data generated with RNA-seq technology}
}}

\item{is.logged}{logical, whether the original data is already logged. If
\code{is.logged = FALSE}, the data is internally transformed.}

\item{mean.variance}{character string indicating whether the mean-variance
relationship should be modeled with precision weights (\code{mean.variance =
"weights"}) or with an empirical Bayes prior trend (\code{mean.variance =
"ebayes"}).}

\item{span}{
    width of the smoothing window used for the lowess mean-variance trend.
    Expressed as a proportion between 0 and 1.}

\item{method}{fitting method; \code{"ls"} for least squares or \code{"robust"} for robust regression}

\item{design}{the design matrix of the microarray experiment, with rows corresponding to samples and columns to coefficients to be estimated.
  Defaults to \code{object$design} if that is non-NULL, otherwise to the unit vector meaning that all samples will be treated as replicates of a single treatment group.}

\item{weights}{non-negative precision weights.  Can be a numeric matrix of individual weights of same size as the object expression matrix, or a numeric vector of array weights with length equal to \code{ncol} of the expression matrix, or a numeric vector of gene weights with length equal to \code{nrow} of the expression matrix.}

\item{ndups}{positive integer giving the number of times each distinct probe is printed on each array.}

\item{spacing}{positive integer giving the spacing between duplicate occurrences of the same probe, \code{spacing=1} for consecutive rows.}

\item{block}{vector or factor specifying a blocking variable on the arrays. Has length equal to the number of arrays. Must be \code{NULL} if \code{ndups>2}.}

\item{correlation}{the inter-duplicate or inter-technical replicate correlation}

\item{contrasts}{numeric matrix with rows corresponding to coefficients in \code{fit} and columns containing contrasts. May be a vector if there is only one contrast. \code{NA}s are not allowed.}

\item{proportion}{numeric value between 0 and 1, assumed proportion of genes which are differentially expressed}

\item{stdev.coef.lim}{numeric vector of length 2, assumed lower and upper limits for the standard deviation of log2-fold-changes for differentially expressed genes}

\item{robust}{logical, should the estimation of \code{df.prior} and \code{var.prior} be robustified against outlier sample variances?}

\item{winsor.tail.p}{numeric vector of length 1 or 2, giving left and right tail proportions of \code{x} to Winsorize. Used only when \code{robust=TRUE}.}

\item{coef}{column number or column name specifying which coefficient or contrast of the linear model is of interest. For \code{topTable}, can also be a vector of column subscripts, in which case the gene ranking is by F-statistic for that set of contrasts.}

\item{adjust.method}{method used to adjust the p-values for multiple testing.  Options, in increasing conservatism, include \code{"none"}, \code{"BH"}, \code{"BY"} and \code{"holm"}.
  See \code{\link{p.adjust}} for the complete list of options. A \code{NULL} value will result in the default adjustment method, which is \code{"BH"}.}

\item{logger}{a \code{\link{Logger}} object.}

\item{...}{further arguments to \code{\link[limma]{lmFit}}.}
}
\value{
A list containing two elements:

\describe{
\item{statistic}{A numeric vector, the values of the test statistic}
\item{significance}{A numeric vector, the p-values of the selected test}
}
}
\description{
This function implements a common workflow to fit linear models
and compute moderated t-statistics and moderated F-statistic by empirical Bayes
moderation of the standard errors towards a global value.

If the input data was generated with microarray technology, the following
steps are executed:

\enumerate{
\item linear model fitting for each feature: \code{\link[limma]{lmFit}}
\item compute coefficients and standard errors for a given set of contrasts: \code{\link[limma]{contrasts.fit}}
\item compute moderated t-statistics and moderated F-statistic: \code{\link[limma]{eBayes}}
\item summarise the linear model fit: \code{\link[limma]{topTable}}
}

If the input data was generated with RNA-seq technology, the following
steps are executed:

\enumerate{
\item (optional) transform count data for linear modelling: \code{\link[limma]{voom}}
\item linear model fitting for each feature: \code{\link[limma]{lmFit}}
\item compute coefficients and standard errors for a given set of contrasts: \code{\link[limma]{contrasts.fit}}
\item compute moderated t-statistics and moderated F-statistic: \code{\link[limma]{eBayes}}
\item summarise the linear model fit: \code{\link[limma]{topTable}}
}

For complete details of each step see the manual pages of the respective
functions.

See the \strong{Details} section below for further information.
}
\examples{
#Seed
set.seed(1010)

#Define row/col size
nr = 20
nc = 20

#Data
x = matrix(
 data = stats::rnorm(n = nr*nc),
 nrow = nr,
 ncol = nc,
 dimnames = list(
   paste0("g",seq(nr)),
   paste0("S",seq(nc))
 )
)

#Categorical output vector (binomial)
y = c(rep("wt",nc/2), rep("mut",nc/2))
names(y) = paste0("S",seq(nc))
rowEBayesStatistics(x=x,y=y)


#Categorical output vector (multinomial)
y = sample(x = c("I","II","III"),size=nc,replace=TRUE)
names(y) = paste0("S",seq(nc))
rowEBayesStatistics(x=x,y=y)

#Numerical output vector
y = sample(x = seq(from=20,to=80),size=nc,replace=TRUE)
names(y) = paste0("S",seq(nc))
rowEBayesStatistics(x=x,y=y,statistic='moderated.t')

#Design matrix
y = matrix(
 data = c(
    c(rep(1,nc/2), rep(0,nc/2)),
    c(rep(0,nc/2), rep(1,nc/2))
 ),
 nrow = nc,
 ncol = 2,
 dimnames = list(
   paste0("S",seq(nc)),
   c("wt", "mut")
 )
)
rowEBayesStatistics(x=x,y=y)

#Numerical matrix
y = matrix(
 data = c(
    sample(x = seq(from=20,to=80),size=nc,replace=TRUE),
    sample(x = c(0,1),size=nc,replace=TRUE)
 ),
 nrow = nc,
 ncol = 2,
 dimnames = list(
   paste0("S",seq(nc)),
   c("age", "p53mut")
 )
)
rowEBayesStatistics(x=x,y=y)

}
\author{
Alessandro Barberis
}
