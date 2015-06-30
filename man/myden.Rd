\name{myden}
\alias{myden}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create Density Graph
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
myden(V = d$V, Z = d$Z, Y, v, glab = c("Non-democracy", "Democracy"), color1 = "orange1", color1b = "orange3", color2 = "steelblue4", ylab = "Density", alpha = 0.75, adjust = 1.5, mytheme = theme_bb(), x.limits = NULL, x.breaks = NULL, x.labels = NULL, title, xlab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{V}{
%%     ~~Describe \code{V} here~~
}
  \item{Z}{
%%     ~~Describe \code{Z} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{v}{
%%     ~~Describe \code{v} here~~
}
  \item{glab}{
%%     ~~Describe \code{glab} here~~
}
  \item{color1}{
%%     ~~Describe \code{color1} here~~
}
  \item{color1b}{
%%     ~~Describe \code{color1b} here~~
}
  \item{color2}{
%%     ~~Describe \code{color2} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{adjust}{
%%     ~~Describe \code{adjust} here~~
}
  \item{mytheme}{
%%     ~~Describe \code{mytheme} here~~
}
  \item{x.limits}{
%%     ~~Describe \code{x.limits} here~~
}
  \item{x.breaks}{
%%     ~~Describe \code{x.breaks} here~~
}
  \item{x.labels}{
%%     ~~Describe \code{x.labels} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
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
function (V = d$V, Z = d$Z, Y, v, glab = c("Non-democracy", "Democracy"), 
    color1 = "orange1", color1b = "orange3", color2 = "steelblue4", 
    ylab = "Density", alpha = 0.75, adjust = 1.5, mytheme = theme_bb(), 
    x.limits = NULL, x.breaks = NULL, x.labels = NULL, title, 
    xlab) 
{
    V <- as.numeric(eval(V))
    Z <- eval(Z)
    Y <- eval(Y)
    data <- data.frame(Z[V == v], Y[V == v])
    names(data) <- c("x", "y")
    if (is.null(x.breaks) & is.null(x.labels) & is.null(x.limits)) {
        ggplot(data, aes(x = y, fill = x)) + geom_density(alpha = alpha, 
            adjust = adjust) + geom_vline(xintercept = mean(data$y[data$x == 
            "Non-democracy"], na.rm = T), linetype = "longdash", 
            colour = color1b, size = 1) + geom_vline(xintercept = mean(data$y[data$x == 
            "Democracy"], na.rm = T), linetype = "longdash", 
            colour = color2, size = 1) + ylab(ylab) + xlab(xlab) + 
            mytheme + ggtitle(title) + scale_fill_manual(name = "Regime Type", 
            values = c(color1, color2))
    }
    else {
        ggplot(data, aes(x = y, fill = x)) + geom_density(alpha = alpha, 
            adjust = adjust) + geom_vline(xintercept = mean(data$y[data$x == 
            "Non-democracy"], na.rm = T), linetype = "longdash", 
            colour = color1b, size = 1) + geom_vline(xintercept = mean(data$y[data$x == 
            "Democracy"], na.rm = T), linetype = "longdash", 
            colour = color2, size = 1) + ylab(ylab) + scale_x_continuous(limits = x.limits, 
            breaks = x.breaks, labels = x.labels) + xlab(xlab) + 
            mytheme + ggtitle(title) + scale_fill_manual(name = "Regime Type", 
            values = c(color1, color2))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
