\name{aplDecode}
\alias{aplDecode}
\title{
APL Decode
}
\description{
Convert array indices into vector index.
}
\usage{
    aplDecode(cell, dims)
}

\arguments{
  \item{cell}{A integer vector of length m, where 1<= cell[i]<=dims[i].}
  \item{dims}{A integer vector of length m.}
}
\details{
  aplDecode(x,y) returns an integer k such that a[k] is the
element with index vector x in an m-array in R with dim(a) equal
to y.

The R implementation, which uses column-major ordering, will give results different from the APL implementation. In APL the expression 3 3 31 2 3 evaluates to 18, while aplDecode(1:3,rep(3,3)) gives 22.
}
\references{
G. Helzer. (1989):
\emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
}
\author{
  Jan de Leeuw \email{deleeuw@stat.ucla.edu},
  Masanao Yajima \email{yajima@stat.ucla.edu}
}

\seealso{
    \code{\link[apl]{aplEncode}}
}
\examples{
sapply(1:12,function(x) aplDecode(aplEncode(x,c(2,2,3)),c(2,2,3)))
}
\keyword{ manip }
