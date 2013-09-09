\name{aplExpand}
\alias{aplExpand}
\alias{aplEXV}
\title{
APL Expand
}
\description{
APL expand "\" function 
}
\usage{
aplExpand(x, y, axis = 1)
}
\arguments{
  \item{x}{An array.}
  \item{y}{A boolean pattern.  Number of TRUE element must match the dimension of x in. }
  \item{axis}{A direction of an axis to expand.}
}
\details{
The elements of an array x is expanded in the pattern of y in the direction specified by axis.
For each element of FALSE, a new element is inserted.  
The number of TRUE elements in the pattern must match the dimension of x in the axis dimension.  
That is \code{dim(x)[axis]} must be the same as \code{sum(y==TRUE)}.
}
\references{
G. Helzer. (1989):
\emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
}
\author{
  Jan de Leeuw \email{deleeuw@stat.ucla.edu}
}
\seealso{
    \code{\link[apl]{aplReduce}}}
\examples{
 y<-c(TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE)
 x<-matrix(1:15, 3, 5)
 z<-c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE)
 aplExpand(x,y)
 aplExpand(x,z,axis=2)
}
\keyword{ manip }
