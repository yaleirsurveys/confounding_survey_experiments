\name{myreg}
\alias{myreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Regression Analysis
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
myreg(Y, Z = d$Z, V = d$V, row = 2, print.out = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{Z}{
%%     ~~Describe \code{Z} here~~
}
  \item{V}{
%%     ~~Describe \code{V} here~~
}
  \item{row}{
%%     ~~Describe \code{row} here~~
}
  \item{print.out}{
%%     ~~Describe \code{print.out} here~~
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
function (Y, Z = d$Z, V = d$V, row = 2, print.out = TRUE) 
{
    Y <- as.numeric(eval(Y))
    Z <- as.numeric(eval(Z))
    V <- as.numeric(eval(V))
    res <- matrix(NA, max(V), 2)
    for (i in 1:max(V)) {
        regout <- robustse(lm(Y[V == i] ~ Z[V == i]))
        res[i, ] <- regout[row, 1:2]
        if (print.out) {
            print(regout)
        }
    }
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
