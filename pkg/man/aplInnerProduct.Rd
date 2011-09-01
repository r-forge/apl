\name{aplInnerProduct}
\alias{aplInnerProduct}
\title{
APL Inner Product
}
\description{
Generalized inner product.
}
\usage{
aplInnerProduct(a, b, f = "*", g = "+")
}
\arguments{
  \item{a}{An array of dimension I * J * ... * K. }
  \item{b}{An array of dimension K * L * ... * M.}
  \item{f}{A function to apply across.  The default is multiplication "*".}
  \item{g}{A function to apply within.  The default is addition "+".}
}
\details{
This function is a generalized version of an array inner product \%*\% for multidimensional arrays.  An array $a$ and an array $b$ must have the same size for the last dimension of $a$ and for the first dimension of $b$.  By default, the function performs the usual inner product; each element of the array $a$ and corresponding element of the array $b$ is multiplied and the result is summed over.  This operation, however, can be generalized by specifying different function for the options $f$ and $g$.
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
    \code{\link[apl]{aplOuterProduct}}
}
\examples{
a <- matrix( 1:9, 3, 3 )
b <- matrix( 9:20, 3, 4 )
aplInnerProduct( a, b )
a%*%b
}
\keyword{ manip }
