\name{aplTranspose}
\alias{aplTranspose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aplTranspose(a, x = rev(1:aplRank(a)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function(a,x=rev(1:aplRank(a))) {
sa<-aplShape(a); ra<-aplRank(a); na<-prod(sa)
if (length(x) != ra)
    stop("Length Error")
rz<-max(x); sz<-rep(0,rz)
for (i in 1:rz)
    sz[i]<-min(sa[which(x==i)])
nz<-prod(sz); z<-array(0,sz)
array(.C("aplTransposeC",as.double(a),as.integer(x),as.integer(sa),as.integer(ra),as.integer(na),
	as.integer(sz),as.integer(rz),as.integer(nz),as.double(z))[[9]],sz)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
