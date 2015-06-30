\name{manualiv}
\alias{manualiv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performs 2SLS
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
manualiv(instrument, treatment, outcome)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{instrument}{
%%     ~~Describe \code{instrument} here~~
}
  \item{treatment}{
%%     ~~Describe \code{treatment} here~~
}
  \item{outcome}{
%%     ~~Describe \code{outcome} here~~
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
function (instrument, treatment, outcome) 
{
    first <- lm(treatment ~ instrument)
    treatment.hat <- first$fitted
    second <- lm(outcome ~ treatment.hat)
    X.adj <- cbind(rep(1, length(treatment)), treatment)
    second$residuals <- outcome - X.adj \%*\% coef(second)
    return(robustse(second))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
