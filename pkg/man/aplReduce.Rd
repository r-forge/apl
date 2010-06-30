\name{aplReduce}
\alias{aplReduce}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aplReduce(a, k, f = "+")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{f}{
%%     ~~Describe \code{f} here~~
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
function(a,k,f="+") {
if (is.vector(a)) 
    return(aplRDV(a,f))
ff<-if (is.function(f)) f else match.fun(f)
sa<-aplShape(a); ra<-aplRank(a); na<-prod(sa)
sz<-sa[(1:ra)[-k]]; z<-array(0,sz); rz<-aplRank(z); nz<-prod(sz)
z<-.C("aplReduceC",list(ff),as.double(a),as.integer(k),as.integer(length(k)),as.integer(na),
    as.integer(sa),as.integer(ra),as.integer(nz),as.integer(sz),as.integer(rz),as.double(z))
return(array(z[[11]],sz))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
