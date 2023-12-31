% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/8-filter-functions.R
\name{rowFilterByMedianAboveMinExpr}
\alias{rowFilterByMedianAboveMinExpr}
\title{Filter by Median Above Minimum Expression}
\usage{
rowFilterByMedianAboveMinExpr(x, g = NULL, min.expr = 0)
}
\arguments{
\item{x}{\code{matrix} or \code{data.frame}, where rows are features and columns are observations.}

\item{g}{(optional) vector or factor object giving the group for the corresponding
elements of \code{x}.}

\item{min.expr}{numerical value indicating the median minimum expression required.}
}
\value{
A logical vector of length \code{nrow(x)} indicating which rows of
\code{x} passed the filter.
}
\description{
This function filters the input matrix \code{x} depending on the
features' intensity.
Variables are removed if their median values are lower than a provided minimum.

See the \strong{Details} section below for further information.
}
\details{
If \code{g = NULL}, the median of each feature is computed across all
observations via \code{\link[matrixStats]{rowMedians}}.
Then, the i-th feature is kept if \eqn{median_{i} >= min.expr}.

If \code{g} is provided, the median per group is computed for each feature via
\code{\link[stats]{median}}.

Then, the i-th feature is kept if \eqn{median_{ig} >= min.expr} in at least
one group.
}
\examples{
#Seed
set.seed(1010)

#Define row/col size
nr = 5
nc = 10

#Data
x = matrix(
 data = sample.int(n = 100, size = nr*nc, replace = TRUE),
 nrow = nr,
 ncol = nc,
 dimnames = list(
   paste0("f",seq(nr)),
   paste0("S",seq(nc))
 )
)

#Grouping variable
g = c(rep("a", nc/2), rep("b", nc/2))

#Filter
rowFilterByMedianAboveMinExpr(x)

#Filter by group
rowFilterByMedianAboveMinExpr(x = x, g = g)

#Set 1st feature to 0s for 2/3 observations
x[1,seq(2*nc/3)] = 0

#Set 2nd feature to 0s for 2/3 observations of class "a"
x[2,seq(2*nc/6)] = 0

#Set 3rd feature to 0s for 2/3 observations of class "a" and "b"
x[3,seq(2*nc/6)] = 0
x[3,(seq(2*nc/6)+nc/2)] = 0

#Filter (1st and 3rd features should be flagged to be removed)
rowFilterByMedianAboveMinExpr(x = x, min.expr = 1)


#Filter by group (3rd feature should be flagged to be removed)
rowFilterByMedianAboveMinExpr(x = x, g = g, min.expr = 1)

}
\author{
Alessandro Barberis
}
