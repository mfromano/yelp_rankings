\name{f.prime}
\alias{f.prime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
f.prime(graph, Wmat, user)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{Wmat}{
%%     ~~Describe \code{Wmat} here~~
}
  \item{user}{
%%     ~~Describe \code{user} here~~
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
function (graph, Wmat, user) 
{
    nbhd <- neighbors(graph = graph, v = user)
    offset <- length(which(V(graph)$isuser \%in\% 1))
    object.list <- which(V(graph)$type \%in\% 1) - offset
    nbhd.ind <- which(V(graph) \%in\% nbhd) - offset
    fp <- sapply(object.list, function(obj) {
        return(sum(Wmat[obj, nbhd.ind]))
    })
    vs <- which(V(graph)$type \%in\% 1)
    names(fp) <- names(V(graph)[vs])
    fp <- fp[!(names(fp) \%in\% names(neighbors(graph = graph, 
        v = user)))]
    fp <- sort(fp, decreasing = TRUE)
    return(fp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
