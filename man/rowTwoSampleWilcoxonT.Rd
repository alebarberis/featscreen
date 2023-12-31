% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3-stats-test-functions.R
\name{rowTwoSampleWilcoxonT}
\alias{rowTwoSampleWilcoxonT}
\title{Two-Sample Wilcoxon Rank Sum and Signed Rank Tests}
\usage{
rowTwoSampleWilcoxonT(
  x,
  g,
  alternative = c("two.sided", "greater", "less"),
  paired = FALSE,
  null = 0,
  exact = NA,
  correct = TRUE
)
}
\arguments{
\item{x}{\code{matrix} or \code{data.frame}.}

\item{g}{a vector or factor object giving the group for the corresponding
elements of \code{x}.}

\item{alternative}{character string or vector of length \code{nrow(x)}.
The alternative hypothesis for each row of \code{x}.
Values must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.}

\item{paired}{logical, whether to compute a paired Wilcoxon test.}

\item{null}{numerical value or numeric vector of length \code{nrow(x)}.
The true values of the difference in means between the two groups of
observations for each row.}

\item{exact}{logical or NA (default) indicator whether an exact p-value
should be computed (see Details).
A single value or a logical vector with values for each observation.}

\item{correct}{logical indicator whether continuity correction should be
applied in the cases where p-values are obtained using normal approximation.
A single value or logical vector with values for each observation.}
}
\value{
A list containing two elements:

\describe{
\item{statistic}{A numeric vector, the values of the test statistic}
\item{significance}{A numeric vector, the p-values of the selected test}
}
}
\description{
Computes the two-sample Wilcoxon test for each feature.

See the \strong{Details} section below for further information.
}
\details{
The function internally calls \code{\link[matrixTests]{row_wilcoxon_paired}}
for the paired test, and \code{\link[matrixTests]{row_wilcoxon_twosample}} for
the unpaired test.
}
\examples{
#Seed
set.seed(1010)

#Data
x = matrix(rnorm(100 * 20), 100, 20)
g = c(rep(0,10),rep(1,10))

#Compute
rowTwoSampleWilcoxonT(x = x, g = g)

#Compute paired
rowTwoSampleWilcoxonT(x = x, g = g, paired = TRUE)

}
\author{
Alessandro Barberis
}
