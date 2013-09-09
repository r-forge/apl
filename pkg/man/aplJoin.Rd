\name{aplJoin}
\alias{aplJoin}
\title{
APL Join
}
\description{
Joins two arrays together along specified dimension.
}
\usage{
aplJoin(a, b, k)
}
\arguments{
  \item{a}{An array.}
  \item{b}{An array.}
  \item{k}{A integer specifying a dimension to join arrays $a$ and $b$ along.}
}
\details{
Arrays $a$ and $b$ are joined along the dimension $k$.  
If $a$ is n by m array and $b$ is k by j array to join $a$ and $b$ along first dimension, m must be same as j.  
Similarly, to join along second dimension, n must be same as k.
\code{aplJoin(a,b,k)} is the equivalent of APL \code{a,[k] b}.
}
\references{
G. Helzer. (1989):
\emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
}
\author{
  Jan de Leeuw \email{deleeuw@stat.ucla.edu}
}

\examples{
a<- array(1:6,c(2,3))
b<- array(7:12,c(2,3))
aplJoin(a,b,1)
a<- array(1:6,c(2,3))
b<- array(1:4,c(2,2))
aplJoin(a,b,2)
}
\keyword{ manip }
