\name{aplShape}
\alias{aplShape}
\title{
APL Shape
}
\description{
Number of components in each dimension of an array.
}
\usage{
aplShape(a)
}
\arguments{
  \item{a}{An array.}
}
\details{This function returns sizes of each dimensions of given array.}

\references{
G. Helzer. (1989):
\emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
}
\author{
  Jan de Leeuw \email{deleeuw@stat.ucla.edu}
}

\seealso{
    \code{\link[apl]{aplReshape}}, 
    \code{\link[apl]{aplRank}}
}
\examples{
aplShape(array(1,c(1,2,3)))
}
\keyword{ manip }
