\name{similarity.function}
\alias{similarity.function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
similarity.function(user_i, user_j, graph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{user_i}{
%%     ~~Describe \code{user_i} here~~
}
  \item{user_j}{
%%     ~~Describe \code{user_j} here~~
}
  \item{graph}{
%%     ~~Describe \code{graph} here~~
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
function (user_i, user_j, graph) 
{
    n1 <- neighbors(graph = graph, v = user_i)
    n2 <- neighbors(graph = graph, v = user_j)
    s.numerator <- length(intersect(n1, n2))
    s.denominator <- min(length(n1), length(n2))
    return(s.numerator/s.denominator)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
