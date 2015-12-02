\name{split.graphs}
\alias{split.graphs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
split.graphs(threshold = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function (threshold = 10) 
{
    load(file = paste("review_edges_ds", toString(threshold), 
        ".RData", sep = ""))
    rand.list <- sample(c(1:length(E(gr))), floor(length(E(gr)) * 
        0.9), replace = FALSE)
    train.list <- E(gr)[rand.list]
    probe.list <- setdiff(E(gr), train.list)
    train.graph <- subgraph.edges(graph = gr, eids = train.list)
    probe.graph <- subgraph.edges(graph = gr, eids = probe.list)
    save(train.graph, file = paste("review_edges_train", toString(threshold), 
        ".RData", sep = ""))
    save(probe.graph, file = paste("review_edges_probe", toString(threshold), 
        ".RData", sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
