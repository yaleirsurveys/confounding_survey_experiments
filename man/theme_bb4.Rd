\name{theme_bb4}
\alias{theme_bb4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ggplot2 theme bb4
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
theme_bb4(base_size = 12, base_family = "", height, width)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base_size}{
%%     ~~Describe \code{base_size} here~~
}
  \item{base_family}{
%%     ~~Describe \code{base_family} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
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
function (base_size = 12, base_family = "", height, width) 
{
    theme_grey(base_size = base_size, base_family = base_family) \%+replace\% 
        theme(axis.text = element_text(size = rel(0.8)), legend.position = "right", 
            axis.ticks = element_line(colour = "black"), legend.key = element_rect(colour = "grey80"), 
            panel.background = element_rect(fill = "white", colour = NA), 
            panel.border = element_rect(fill = NA, colour = "grey50"), 
            panel.grid.major = element_line(colour = "grey90", 
                size = 0.2), panel.grid.minor = element_line(colour = "grey98", 
                size = 0.5), strip.background = element_rect(fill = "grey80", 
                colour = "grey50"), strip.background = element_rect(fill = "grey80", 
                colour = "grey50"), legend.key.height = unit(height, 
                "cm"), legend.key.width = unit(width, "cm"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
