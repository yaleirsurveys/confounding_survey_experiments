\name{region.lab}
\alias{region.lab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Labels regions
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
region.lab(region1, region2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{region1}{
%%     ~~Describe \code{region1} here~~
}
  \item{region2}{
%%     ~~Describe \code{region2} here~~
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
function (region1, region2) 
{
    t <- ifelse(eval(region1) == 1 | eval(region2) == 1, 1, 0)
    t[is.na(t)] <- 0
    return(t)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
