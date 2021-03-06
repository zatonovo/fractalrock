% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ifs.R
\name{sierpinski4}
\alias{sierpinski4}
\title{Generate the Sierpinski Carpet}
\usage{
sierpinski4(x)
}
\arguments{
\item{x}{A 2-by-n matrix representing a set of points}
}
\description{
This function generates a single iteration of the Sierpinski Carpet,
which replaces a square with 8 smaller squares on the inside
perimeter of the original square.
}
\examples{
library(lambda.tools)
x <- polygon(4)
xn <- fold(1:5, function(a,b) sierpinski4(b), x)
}
\author{
Brian Lee Yung Rowe
}
\seealso{
\code{\link{sierpinski3}} \code{\link{sierpinski5}}
}
\keyword{math}

