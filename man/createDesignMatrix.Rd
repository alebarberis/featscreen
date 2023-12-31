% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/4-biostats-test-functions.R
\name{createDesignMatrix}
\alias{createDesignMatrix}
\title{Create a design matrix}
\usage{
createDesignMatrix(y)
}
\arguments{
\item{y}{a \code{vector}, \code{factor} or \code{matrix}}
}
\value{
A \code{matrix}.
}
\description{
Simple function to generate a design matrix when the input is a
vector or a factor. If the input is a matrix, it is returned as is.
See the \strong{Details} section below for further information.
}
\details{
This function is internally used to create a design matrix when not
directly provided.
}
\examples{
\dontrun{
#Categorical vector
y = c(rep("a",5), rep("b",5))
names(y) = paste0("s",seq(10))
createDesignMatrix(y)

#Factor
y = as.factor(y)
createDesignMatrix(y)

#Numerical vector with 2 categories
y = c(rep(1,5), rep(0,5))
names(y) = paste0("s",seq(10))
createDesignMatrix(y)

#Numerical vector
y = sample(x = seq(from=20,to=80),10)
names(y) = paste0("s",seq(10))
createDesignMatrix(y)

#Design matrix
y = matrix(
 data = c(
    1,1,1,0,0,0,
    0,0,0,1,1,1
 ),
 nrow = 6,
 ncol = 2,
 dimnames = list(
   paste0("S",seq(6)),
   c("wt", "mut")
 )
)
createDesignMatrix(y)

#Numerical matrix
y = matrix(
 data = c(
    sample(x = seq(from=20,to=80),6),
    0,0,0,1,1,1
 ),
 nrow = 6,
 ncol = 2,
 dimnames = list(
   paste0("S",seq(6)),
   c("age", "p53mut")
 )
)
createDesignMatrix(y)
}
}
\author{
Alessandro Barberis
}
\keyword{internal}
