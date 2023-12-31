% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3-stats-test-functions.R
\name{rowPearsonChiSq}
\alias{rowPearsonChiSq}
\title{Pearson's Chi-squared Test of Independence}
\usage{
rowPearsonChiSq(x, g, correct = TRUE, simulate.p.value = FALSE, B = 2000)
}
\arguments{
\item{x}{\code{matrix} or \code{data.frame}.}

\item{g}{a vector or factor object giving the group for the corresponding
elements of \code{x}.}

\item{correct}{a logical indicating whether to apply continuity
    correction when computing the test statistic for 2 by 2 tables: one
    half is subtracted from all \eqn{|O - E|} differences; however, the
    correction will not be bigger than the differences themselves.  No correction
    is done if \code{simulate.p.value = TRUE}.}

\item{simulate.p.value}{a logical indicating whether to compute
    p-values by Monte Carlo simulation.}

\item{B}{an integer specifying the number of replicates used in the
    Monte Carlo test.}
}
\value{
A list containing two elements:

\describe{
\item{statistic}{A numeric vector, the values of the test statistic}
\item{significance}{A numeric vector, the p-values of the selected test}
}
}
\description{
Computes the Pearson's Chi-squared test of independence for each
row vector in \code{x}.

See the \strong{Details} section below for further information.
}
\details{
It is a wrapper to \code{\link[stats]{chisq.test}} function.
}
\examples{
#Seed
set.seed(1010)

#Data
x = rbind(
  matrix(sample(c("mut", "wt"),30,TRUE), 1, 30),
  matrix(sample(c("m", "f")   ,30,TRUE), 1, 30)
)
g = sample(c("a","b","c"), 30, replace = TRUE)

#Compute
rowPearsonChiSq(x = x, g = g, simulate.p.value = TRUE)

}
\author{
Alessandro Barberis
}
