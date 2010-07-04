\name{aplExpand}
\alias{aplExpand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
APL Expand
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aplExpand(x, y, axis = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{axis}{
%%     ~~Describe \code{axis} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
G. Helzer. (1989):
\emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
}
\author{
  Jan de Leeuw \email{deleeuw@stat.ucla.edu}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x,y,axis=1) {
    if (is.vector(x)) return(aplEXV(x,y))
    d<-dim(x); m<-which(y); e<-d; e[axis]<-m
    if (m != d[axis])
        stop("Incorrect dimension length")
    z<-array(0,e)
    apply(z,(1:n)[-axis],function(i) z[i]<-x[i])
  }
}
\keyword{ manip }
