\name{collaborative.filter}
\alias{collaborative.filter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
collaborative.filter(user, train.graph, similarity_matrix, threshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{user}{
%%     ~~Describe \code{user} here~~
}
  \item{train.graph}{
%%     ~~Describe \code{train.graph} here~~
}
  \item{similarity_matrix}{
%%     ~~Describe \code{similarity_matrix} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
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
function (user, train.graph, similarity_matrix, threshold) 
{
    cf.ps.handle <- Curry(cf.predicted.score, graph = train.graph, 
        user = user, S = similarity_matrix)
    restaurant.indices <- which(V(train.graph)$type \%in\% 1)
    pth <- proc.time()
    outarray <- mclapply(restaurant.indices, cf.ps.handle, mc.preschedule = TRUE, 
        mc.cores = detectCores())
    print(proc.time() - pth)
    businesses <- names(V(train.graph)[restaurant.indices])
    names(outarray) <- businesses
    ratings.list <- outarray
    save(ratings.list, file = paste("cf_ratings/cf_ratings_user", 
        toString(user), "_", toString(threshold), ".RData", sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
