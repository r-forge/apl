\name{first}
\alias{first}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
First element in a vector   
}
\description{
Returns the first element in a vector.
}
\usage{
first(x)
}
\arguments{
  \item{x}{a vector}
}
\details{
This function takes a vector and returns only the first element.
}
\author{
  Jan de Leeuw \email{deleeuw@stat.ucla.edu}
}
\seealso{
    \code{\link[apl]{last}}, 
    \code{\link[apl]{butFirst}}, 
    \code{\link[apl]{butLast}}
}
\examples{
    x <- 1:10
    first(x)
}
\keyword{manip}

