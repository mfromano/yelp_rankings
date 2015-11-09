graph.threshold <- function(gr, threshold)
{
    vids <- V(gr)[graph.strength(gr) > threshold]
    vids <- vids[vids$type == FALSE]
    return(induced.subgraph(gr,vids=unique(unlist(neighborhood(gr,order=1,nodes=vids)))))
}

# TODO: optimize the following function!!!
# this is the resource of j -> i
w.ij <- function(graph, i, j)
{
    k.xj <- graph.strength(graph,vids=j)[[1]]

    objects.in.common <- intersect(neighborhood(graph=graph,order=1,nodes=i)[[1]],neighborhood(graph=graph,order=1,nodes=j)[[1]])
    # for each object
    if (length(objects.in.common) > 0)
    {
        total <- sum(1/graph.strength(graph=graph,vids=objects.in.common))
        return(total/k.xj)
    }
    else
    {
        return(0)
    }
}

get.W <- function(vids, graph)
{
    W <- Matrix(0,length(vids),length(vids))
    for (i in c(1:length(vids))) 
    {
        pth <- proc.time()
        W[i,] <- unlist(mclapply(vids, w.ij, graph=graph, i=vids[i], mc.preschedule=TRUE, mc.cores=detectCores()))
        # W[i,] <- unlist(lapply(vids, w.ij, graph=graph, i=i))   
        print(proc.time() - pth)
    }
    return(W)
}

# Now make a function that recreates formula (12) from Zhou 2007
# for a given user, get f'(o_j) for each object (restaurant)
f.prime <- function(graph, Wmat, user)
{
    # first get neighbors (objects) of user (restaurants that user likes)
    nbhd <- neighbors(graph=graph,v=user)
    # next, for a given object, get the sum of the similarities it has to the
    # restaurants that the user likes
    offset <- length(which(V(graph)$isuser %in% 1))
    object.list <- which(V(graph)$type %in% 1)-offset
    nbhd.ind <- which(V(graph) %in% nbhd)-offset
    fp <- sapply(object.list, function(obj) {return(sum(Wmat[obj,nbhd.ind]))})
    vs <- which(V(graph)$type %in% 1)
    names(fp) <- names(V(graph)[vs])
    fp <- fp[!(names(fp) %in% names(neighbors(graph=graph,v=user)))]
    fp <- sort(fp, decreasing=TRUE)
    return(fp)
}

user.strength <- function(gr, bins=100)
{
    vids <- V(gr)[V(gr)$type == FALSE]
    hist(vids,breaks=bins)
    return(graph.strength(gr, vids=vids))
}

# From Zhou 2007
similarity.function <- function(user_i, user_j, graph)
{
    n1 <- neighbors(graph=graph, v=user_i)
    n2 <- neighbors(graph=graph, v=user_j)
    s.numerator <- length(intersect(n1,n2))
    s.denominator <- min(length(n1), length(n2))
    return(s.numerator/s.denominator)
}

# also from Zhou 2007
cf.predicted.score <- function(graph, user, object)
{
    numerator <- 0
    denominator <- 0
    load('similarity_matrix.RData')
    if (get.edge.ids(graph,c(user,object)) == 0)
    {
        user.list <- V(graph)[V(graph)$isuser]
        # similarity_list <- sapply(user.list, similarity.function, user_j=user, graph=graph)
        similarity_list <- S[user,]
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