\name{aplSelect}
\alias{aplSelect}
\title{
APL Select
}
\description{
Selects elements from an array
}
\usage{
aplSelect(a, x, drop = TRUE)
}
\arguments{
  \item{a}{An array.}
  \item{x}{A vector, or a list of vector specifying the index of array to select from.}
  \item{drop}{When set to TRUE, if only one column or row is selected, array information is dropped and a vector is returned.  Default is FALSE. }
}
\details{
This function is general index specification where elements from a is selected as specified by the list x.
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
    \code{\link[apl]{aplTake}}, 
    \code{\link[apl]{aplDrop}}
}
\examples{
    a <- array( 1:24, c( 2, 3, 4 ) )
    x <- list(2, 1:2, c( 1, 4 ))
    print( a[2,1:2,c( 1, 4 )] )
    print( aplSelect( a, x ) )
    x[[1]] <- c( 2, 2 )
    print( a[c( 2, 2 ), 1:2, c( 1, 4 )] )
    print( aplSelect( a, x ) )
}
\keyword{ manip }
