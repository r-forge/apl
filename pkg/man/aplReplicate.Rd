\name{aplReplicate}
\alias{aplReplicate}
\alias{aplCRV}
\title{
APL Replicate
}
\description{
Replication 
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
\keyword{ manip }
