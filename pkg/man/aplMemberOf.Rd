\name{aplMemberOf}
\alias{aplMemberOf}
\title{
APL MemberOf
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aplMemberOf(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
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
function(a,b) {
    if (!identical(typeof(a),typeof(b)))
        warning("Arguments of different types")
    arrTest(a); arrTest(b)
    sa<-aplShape(a); sb<-aplShape(b)
    na<-prod(sa); nb<-prod(sb)
    z<-array(0,sa)
    for (i in 1:na) {
        z[i]<-0; aa<-a[i]
        for (j in 1:nb)
            if (identical(aa,b[j])) z[i]<-1
        }
return(z)   
  }
}
\keyword{ manip }
