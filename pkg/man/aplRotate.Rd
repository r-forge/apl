\name{aplRotate}
\alias{aplRotate}
\alias{aplRTV}
\title{
APL Rotate
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aplRotate(a, x, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function(a,x,k) {
    if (is.vector(a)) return(aplRTV(a,k))
    sa<-aplShape(a); sx<-aplShape(x)
    if (sx == 1) x<-array(x,sa[-k])
    if (!identical(sa[-k],aplShape(x)))
        stop("Index Error")
    z<-array(0,sa); sz<-sa; nz<-prod(sz); sk<-sz[k]
    for (i in 1:nz) {
        ivec<-aplEncode(i,sz)
        xx<-x[aplDecode(ivec[-k],sx)]
        ak<-rep(0,sk)
        for (j in 1:sk) {       
            jvec<-ivec; jvec[k]<-j
            ak[j]<-a[aplDecode(jvec,sz)]
            }       
        bk<-aplRTV(ak,xx)
        for (j in 1:sk) {       
            jvec<-ivec; jvec[k]<-j
            z[aplDecode(jvec,sz)]<-bk[j]
            }       
    }
return(z)
  }
}
\keyword{ manip }
