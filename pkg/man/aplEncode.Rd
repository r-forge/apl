\name{aplEncode}
\alias{aplEncode}
\title{
APL Encode
}
\description{
Converts vector index into array index.
}
\usage{
aplEncode(ind, dims)
}
\arguments{
  \item{ind}{ an integer 1 <= ind <= sum(dims)}
  \item{dims}{ an integer vector of length m }
}
\details{
aplEncode(x,y) returns an m-vector with the indices of a[x] if a is an m-array in R with dim(a) equal to dims.

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
    \code{\link[apl]{aplDecode}}
}

\examples{
t(sapply(1:12,function(x) aplEncode(x,c(2,2,3))))
}
\keyword{ manip }
