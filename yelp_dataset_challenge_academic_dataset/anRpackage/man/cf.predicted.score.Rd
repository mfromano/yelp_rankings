\name{cf.predicted.score}
\alias{cf.predicted.score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cf.predicted.score(graph, user, object, S)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{user}{
%%     ~~Describe \code{user} here~~
}
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{S}{
%%     ~~Describe \code{S} here~~
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
function (graph, user, object, S) 
{
    numerator <- 0
    denominator <- 0
    if (get.edge.ids(graph, c(user, object)) == 0) {
        user.list <- V(graph)[V(graph)$isuser]
        similarity_list <- S[user, ]
        user.neighbors <- intersect(user.list, neighbors(graph = graph, 
            v = V(graph)[object]))
        user.neighbors <- setdiff(user.neighbors, user)
        numerator <- sum(unlist(similarity_list[user.neighbors]))
        denominator <- sum(unlist(similarity_list))
        return(numerator/denominator)
    }
    else {
        return(-1)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
