library(igraph)
library(functional)
library(parallel)
a <- read.csv('review_edges.csv', header=F)
gr <- graph.data.frame(a,directed=F)
E(gr)$weight <- sapply(a$V3, as.numeric)
# get the intersection of "users" and vertices, and set these to TRUE. Set others to false
indices <- sapply(V(gr)$name, is.element, set=a$V1)
# indices <- intersect(V(gr)$name, a$user)
V(gr)[!indices]$type <- TRUE
V(gr)[indices]$type <- FALSE
V(gr)$isuser <- !V(gr)$type

# plot(gr, layout=layout.bipartite)

user.strength <- function(gr, bins=100)
{
    vids <- V(gr)[V(gr)$type == FALSE]
    hist(vids,breaks=bins)
    return(graph.strength(gr, vids=vids))
}

graph.threshold <- function(gr, threshold)
{
    vids <- V(gr)[graph.strength(gr) > threshold]
    vids <- vids[vids$type == FALSE]
    return(induced.subgraph(gr,vids=unique(unlist(neighborhood(gr,order=1,nodes=vids)))))
}

# From Zhou 2007
similarity.function <- function(user_i, user_j, graph)
{
    n1 <- neighbors(graph=graph, v=V(graph)[user_i])
    n2 <- neighbors(graph=graph, v=V(graph)[user_j])
    s.numerator <- length(intersect(n1,n2))
    s.denominator <- min(length(n1), length(n2))
    return(s.numerator/s.denominator)
}

# also from Zhou 2007
cf.predicted.score <- function(graph, user, object)
{
    numerator <- 0
    denominator <- 0

    if (get.edge.ids(graph,c(user,object)) == 0)
    {
        user.list <- V(graph)[V(graph)$isuser]
        similarity_list <- sapply(user.list, similarity.function, user_j=user, graph=graph)
        user.neighbors <- intersect(user.list, neighbors(graph=graph, v=V(graph)[object]))
        user.neighbors <- setdiff(user.neighbors,user)
        numerator <- sum(unlist(similarity_list[user.neighbors]))
        denominator <- sum(unlist(similarity_list))
        return(numerator/denominator)
    }
    else
    {
        # get real recommended score if the user has rated the restaurant
        return(-1)
    }
}

g.thrshld <- graph.threshold(gr, threshold=10)
cf.ps.handle <- Curry(cf.predicted.score, graph=g.thrshld, user=2)
restaurant.indices <- which(V(g.thrshld)$type %in% 1) # gets indices of restaurants
outarray <- mclapply(restaurant.indices, cf.ps.handle, mc.preschedule=TRUE, mc.cores=detectCores())
businesses <- names(V(g.thrshld)[restaurant.indices])
names(outarray) <- businesses
ratings.list <- outarray
write.csv(ratings.list,file='usr2.csv')