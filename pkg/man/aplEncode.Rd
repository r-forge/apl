\name{aplEncode}
\alias{aplEncode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
APL Encode
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aplEncode(ind, dims)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ind}{
%%     ~~Describe \code{ind} here~~
}
  \item{dims}{
%%     ~~Describe \code{dims} here~~
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
function(ind,dims) {
n<-length(dims); cell<-integer(n)
if ((ind < 1) || (ind > prod(dims))) stop("No such cell")
.C("aplEncodeC ",as.integer(cell),as.integer(dims),as.integer(n),as.integer(ind))[[1]]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
