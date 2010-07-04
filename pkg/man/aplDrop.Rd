\name{aplDrop}
\alias{aplDrop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
APL Drop
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aplDrop(a, x, drop = FALSE)
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
y<-as.list(rep(0,ra))
for (i in 1:ra) {
    ss<-sa[i]; xx<-x[i]; sx<-ss-xx
    if (xx >= 0) y[[i]]<-(xx+1):ss
    if (xx < 0) y[[i]]<-1:sx
    }
return(aplSelect(a,y,drop))
  }
}
\keyword{ manip }
