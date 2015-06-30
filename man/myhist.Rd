\name{myhist}
\alias{myhist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates Histogram Graph
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
myhist(X, Y, glab = c("Non-democracy", "Democracy"), color1 = "orange1", color2 = "steelblue4", outline = "black", mytheme = theme_bb(), title, xlab, ylab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{glab}{
%%     ~~Describe \code{glab} here~~
}
  \item{color1}{
%%     ~~Describe \code{color1} here~~
}
  \item{color2}{
%%     ~~Describe \code{color2} here~~
}
  \item{outline}{
%%     ~~Describe \code{outline} here~~
}
  \item{mytheme}{
%%     ~~Describe \code{mytheme} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
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
function (X, Y, glab = c("Non-democracy", "Democracy"), color1 = "orange1", 
    color2 = "steelblue4", outline = "black", mytheme = theme_bb(), 
    title, xlab, ylab) 
{
    data <- na.omit(data.frame(eval(X), eval(Y)))
    data$x <- factor(data[, 1], labels = glab)
    data$y <- factor(data[, 2], labels = c("No", "Yes"))
    ggplot(data = data, aes(x = y, fill = x)) + geom_histogram(position = "dodge", 
        color = outline) + xlab(xlab) + ylab(ylab) + mytheme + 
        ggtitle(title) + scale_fill_manual(values = c(color1, 
        color2), name = "Regime Type")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
