\name{butFirst}
\alias{butFirst}
\title{Everything but the first element}
\description{
Returns everything but the first element in a vector.
}
\usage{
butFirst(x)
}
\arguments{
  \item{x}{a vector}
}
\details{
This function takes a vector and returns the original vector without the first element.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
  Jan de Leeuw \email{deleeuw@stat.ucla.edu}
}
%\note{
%%  ~~further notes~~
%}

\seealso{
    \code{\link[apl]{first}}, 
    \code{\link[apl]{last}}, 
    \code{\link[apl]{butLast}}
}


\examples{
    x <- 1:10
    butFirst(x)
}
\keyword{manip}

