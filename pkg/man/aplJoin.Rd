\name{aplJoin}
\alias{aplJoin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aplJoin(a, b, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
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
function(a,b,k) {
    if (is.vector(a) && is.vector(b)) return(c(a,b))
    sa<-aplShape(a); sb<-aplShape(b); ra<-aplRank(a); rb<-aplRank(b)
    if (ra != rb)
        stop("Rank error in aplJoin")
    if (!identical(sa[-k],sb[-k]))
        stop("Shape error")
    sz<-sa; sz[k]<-sz[k]+sb[k]; nz<-prod(sz); u<-unit(k,ra)
    z<-array(0,sz)
    for (i in 1:nz) {
        ivec<-aplEncode(i,sz)
        if (ivec[k] <= sa[k]) z[i]<-a[aplDecode(ivec,sa)]
            else z[i]<-b[aplDecode(ivec-sa[k]*u,sb)]
        }
return(z)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
