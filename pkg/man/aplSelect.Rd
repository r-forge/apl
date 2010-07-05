\name{aplSelect}
\alias{aplSelect}
\title{
APL Select
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aplSelect(a, x, drop = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{drop}{
%%     ~~Describe \code{drop} here~~
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
function(a,x,drop=FALSE) {
sa<-aplShape(a); ra<-aplRank(a)
sz<-sapply(x,length); z<-array(0,sz); rz<-aplRank(z); nz<-prod(sz)
z<-array(.C("aplSelectC",as.double(a),as.integer(sa),as.integer(ra),lapply(x,as.integer),
    as.double(z),as.integer(sz),as.integer(rz),as.integer(nz))[[5]],sz)
if (drop) return(drop(z)) else return(z)
  }
}
\keyword{ manip }
