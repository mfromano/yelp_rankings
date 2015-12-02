\name{generate.similarityMatrix}
\alias{generate.similarityMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generate.similarityMatrix(gr, outfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gr}{
%%     ~~Describe \code{gr} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
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
function (gr, outfile) 
{
    user.list <- V(gr)[V(gr)$isuser]
    S <- Matrix(0, length(user.list), length(user.list))
    for (i in c(1:length(user.list))) {
        pth <- proc.time()
        S[i, ] <- sapply(user.list, similarity.function, user_j = user.list[i], 
            graph = gr)
        print(proc.time() - pth)
    }
    save(S, file = outfile)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
