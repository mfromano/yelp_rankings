\name{get.W}
\alias{get.W}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.W(vids, graph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vids}{
%%     ~~Describe \code{vids} here~~
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
function (vids, graph) 
{
    W <- Matrix(0, length(vids), length(vids))
    for (i in c(1:length(vids))) {
        pth <- proc.time()
        W[i, ] <- unlist(mclapply(vids, w.ij, graph = graph, 
            i = vids[i], mc.preschedule = TRUE, mc.cores = detectCores()))
        print(proc.time() - pth)
    }
    return(W)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
