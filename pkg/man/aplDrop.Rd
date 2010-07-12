\name{aplDrop}
\alias{aplDrop}
\title{
APL Drop
}
\description{
Remove the first (or last) a elements of x according to sign of a
}
\usage{
aplDrop(a, x, drop = FALSE)
}
\arguments{
  \item{a}{}
  \item{x}{}
  \item{drop}{ Boolean flag to specify whether to convert the result into a vector when it only has one column.  Default if FALSE.}
}

\references{
G. Helzer. (1989):
\emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
}
\author{
  Jan de Leeuw \email{deleeuw@stat.ucla.edu}
}

\seealso{
    \code{\link[apl]{aplTake}}
}
\examples{
aplDrop()
}
\keyword{ manip }
