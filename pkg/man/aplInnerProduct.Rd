\name{aplInnerProduct}
\alias{aplInnerProduct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aplInnerProduct(a, b, f = "*", g = "+")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{g}{
%%     ~~Describe \code{g} here~~
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
function(a,b,f="*",g="+") {
sa<-aplShape(a); sb<-aplShape(b)
ra<-aplRank(a); rb<-aplRank(b)
ia<-1:(ra-1); ib<-(ra-1)+(1:(rb-1))
ff<-match.fun(f); gg<-match.fun(g)
ns<-last(sa); nt<-first(sb)
if (ns != nt) 
    stop("Incompatible array dimensions")
sz<-c(butLast(sa),butFirst(sb)); nz<-prod(sz)
z<-array(0,sz); rz<-aplRank(z)
res<-.C("aplInnerProductC",list(ff,gg),as.double(a),as.double(b),as.integer(sa),as.integer(ra),
	as.integer(sb),as.integer(rb),as.integer(sz),as.integer(rz),as.integer(nz),as.integer(ns),as.double(z))
return(array(res[[12]],sz))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
