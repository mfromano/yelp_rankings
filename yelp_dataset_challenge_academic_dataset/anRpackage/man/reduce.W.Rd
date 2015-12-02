\name{reduce.W}
\alias{reduce.W}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reduce.W(original.graph, new.graph, oldW)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{original.graph}{
%%     ~~Describe \code{original.graph} here~~
}
  \item{new.graph}{
%%     ~~Describe \code{new.graph} here~~
}
  \item{oldW}{
%%     ~~Describe \code{oldW} here~~
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
function (original.graph, new.graph, oldW) 
{
    num.users.original <- length(V(original.graph)[V(original.graph)$isuser])
    objects.original <- V(original.graph)[!V(original.graph)$isuser]
    objects.new <- V(new.graph)[!V(new.graph)$isuser]
    num.users.new <- length(V(new.graph)[V(new.graph)$isuser])
    sd <- setdiff(objects.original, objects.new) - num.users.original
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
