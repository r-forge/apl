\name{last}
\alias{last}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Last element in a vector   
}
\description{
Returns the last element in a vector.
}
\usage{
last(x)
}
\arguments{
  \item{x}{a vector}
}
\details{

This function takes a vector and returns only the last element.
}
\author{
  Jan de Leeuw \email{deleeuw@stat.ucla.edu}
}
\seealso{
    \code{\link[apl]{first}}, 
    \code{\link[apl]{butFirst}}, 
    \code{\link[apl]{butLast}}
}
\examples{
    x <- 1:10
    last(x)
}
\keyword{manip}