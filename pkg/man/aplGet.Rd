\name{aplGet}
\alias{aplGet}
\title{
APL Get
}
\description{
Get elements of an array
}
\usage{
aplGet(a, cell)
}
\arguments{
  \item{a}{An array.}
  \item{cell}{Indices of cell to get the value from.  Length of indices must match the dimension of the array.}
}
\references{
G. Helzer. (1989):
\emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
}
\author{
  Jan de Leeuw \email{deleeuw@stat.ucla.edu}
}
\seealso{
    \code{\link[apl]{aplSet}}
}
\examples{
a<-array(1:24,c(2,3,4))
aplGet(a,c(1,2,4))
a[1,2,4]
}
\keyword{ manip }
