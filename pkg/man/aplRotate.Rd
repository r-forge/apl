\name{aplRotate}
\alias{aplRotate}
\alias{aplRTV}
\title{
APL Rotate
}
\description{
Rotates an array or a vector 
}
\usage{
aplRotate(a, x, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{an array or a vector}
  \item{x}{When a is an array this will not be used.}
  \item{k}{When a is a vector, if a positive number is specified, it represents the 
number of elemets in the head of the vector to be placed at the end of the vector. 
On the other hand when a negative number is specified, it represents the number of 
elements from the tail of the vector to be placed at the head of the vector.}
}

\details{}

\references{
G. Helzer. (1989):
\emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
}

\author{
  Jan de Leeuw \email{deleeuw@stat.ucla.edu}
}

%\seealso{}
\examples{
aplRotate(1:5,1)
aplRotate(1:5,-2)
}
\keyword{ manip }
