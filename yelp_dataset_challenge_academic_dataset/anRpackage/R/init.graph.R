init.graph <-
function(file='review_edges.csv')
{
    a <- read.csv(file, header=F)
    gr <- graph.data.frame(a,directed=F)
    E(gr)$weight <- sapply(a$V3, as.numeric)
    # get the intersection of "users" and vertices, and set these to TRUE. Set others to false
    indices <- sapply(V(gr)$name, is.element, set=a$V1)
    # indices <- intersect(V(gr)$name, a$user)
    V(gr)[!indices]$type <- TRUE
    V(gr)[indices]$type <- FALSE
    V(gr)$isuser <- !V(gr)$type
    return(gr)
}
