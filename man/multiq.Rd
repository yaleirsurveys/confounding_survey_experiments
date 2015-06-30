\name{multiq}
\alias{multiq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
multiq
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
multiq(v1, v2, v3, v4, v5, v6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v1}{
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{
%%     ~~Describe \code{v2} here~~
}
  \item{v3}{
%%     ~~Describe \code{v3} here~~
}
  \item{v4}{
%%     ~~Describe \code{v4} here~~
}
  \item{v5}{
%%     ~~Describe \code{v5} here~~
}
  \item{v6}{
%%     ~~Describe \code{v6} here~~
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
function (v1, v2, v3, v4, v5, v6) 
{
    res <- rep(NA, length(v1))
    res[!is.na(eval(v1))] <- eval(v1)[!is.na(eval(v1))]
    res[!is.na(eval(v2))] <- eval(v2)[!is.na(eval(v2))]
    res[!is.na(eval(v3))] <- eval(v3)[!is.na(eval(v3))]
    res[!is.na(eval(v4))] <- eval(v4)[!is.na(eval(v4))]
    res[!is.na(eval(v5))] <- eval(v5)[!is.na(eval(v5))]
    res[!is.na(eval(v6))] <- eval(v6)[!is.na(eval(v6))]
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
