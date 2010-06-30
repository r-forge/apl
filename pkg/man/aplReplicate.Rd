\name{aplReplicate}
\alias{aplReplicate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aplReplicate(x, y, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
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
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function(x,y,k) {
    if (is.vector(x)) return(aplCRV(x,y))
    sx<-aplShape(x); sy<-aplShape(y); sk<-sx[k]
    if (sy == 1) y<-rep(y,sk)
    if (length(y) != sk)
        stop("Length Error")
    sz<-sx; sz[k]<-sum(y); nz<-prod(sz)
    gg<-aplCRV(1:sk,y)
    z<-array(0,sz)
    for (i in 1:nz){
        jvec<-aplEncode(i,sz)
        jvec[k]<-gg[jvec[k]]
        z[i]<-x[aplDecode(jvec,sx)]
    }
return(z)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
