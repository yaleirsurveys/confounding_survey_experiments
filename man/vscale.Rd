\name{vscale}
\alias{vscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardize placebo variables
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
vscale(var, vig = d$V)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{vig}{
%%     ~~Describe \code{vig} here~~
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
function (var, vig = d$V) 
{
    var <- eval(var)
    vig <- eval(vig)
    sv <- c()
    for (i in 1:length(var)) {
        v <- vig[i]
        sv[i] <- (var[i] - mean(var[vig == v], na.rm = T))/sd(var[vig == 
            v], na.rm = T)
    }
    return(sv)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
