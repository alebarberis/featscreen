% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/5-selection-functions.R
\name{selectByCutoff}
\alias{selectByCutoff}
\title{Select by Cutoff}
\usage{
selectByCutoff(x, cutoff, operator = c("<", "<=", ">", ">="))
}
\arguments{
\item{x}{numerical vector.}

\item{cutoff}{numeric value indicating the cutoff.}

\item{operator}{character string indicating the relational operator to use.}
}
\value{
A logical vector, indicating the elements to keep.
}
\description{
Select elements based on a cutoff on their values.
}
\examples{
#Seed
set.seed(1010)

#Data
x = seq(10)
names(x) = paste0("f", seq(10))

#Select
selectByCutoff(x = x, cutoff = 5, operator = "<")

}
\author{
Alessandro Barberis
}
