library(igraph)
library(functional)
library(parallel)
library(Matrix)

init.graph <- function(file='review_edges.csv')
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

downsample.graph <- function(threshold=10)
{
    gr <- init.graph()
    gr <- simplify(gr)
    gr <- graph.threshold(gr=gr, threshold=threshold)
    save(gr, file=paste('review_edges_ds', toString(threshold), '.RData', sep=''))
}

split.graphs <- function(threshold=10)
{
    load(file=paste('review_edges_ds', toString(threshold), '.RData', sep=''))
    rand.list <- sample(c(1:length(E(gr))), floor(length(E(gr))*.9), replace=FALSE)
    train.list <- E(gr)[rand.list]
    probe.list <- setdiff(E(gr),train.list)
    train.graph <- subgraph.edges(graph=gr, eids=train.list)
    probe.graph <- subgraph.edges(graph=gr, eids=probe.list)
    orphan.nodes <- V(train.graph)[graph.strength(train.graph) == 0]
    train.graph <- delete.vertices(graph=train.graph, v=orphan.nodes)
    # Next three lines delete vertices from probe set that are not in training set
    probe.restaurants <- V(probe.graph)[!V(probe.graph)$isuser]
    unshared.restaurants <- probe.restaurants[which(!names(probe.restaurants) %in% names(V(train.graph)[!V(train.graph)$isuser]))]
    probe.graph <- delete.vertices(graph=probe.graph, v=unshared.restaurants)
    orphan.nodes <- V(probe.graph)[graph.strength(probe.graph) == 0]
    probe.graph <- delete.vertices(graph=probe.graph, v=orphan.nodes)
    save(train.graph, file=paste('review_edges_train',toString(threshold),'.RData', sep=''))
    save(probe.graph, file=paste('review_edges_probe',toString(threshold),'.RData', sep=''))
}

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

    objects.in.common <- which(neighbors(graph=graph,v=i) %in% neighbors(graph=graph,v=j))
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

# returns W matrix with entries i,j corresponding to directed weight from i->j of objects
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

# Modify to code as zeros non-existent entries in
reduce.W <- function(original.graph, new.graph, oldW)
{
    num.users.original <- length(V(original.graph)[V(original.graph)$isuser])
    objects.original <- V(original.graph)[!V(original.graph)$isuser]

    objects.new <- V(new.graph)[!V(new.graph)$isuser]
    num.users.new <- length(V(new.graph)[V(new.graph)$isuser])
    sd <- setdiff(objects.original, objects.new)-num.users.original
}

# '''
# FOUND BUG IN CODE: need to make sure that the indices in Wmat correspond to the
# corresponding indices in the probe matrix!!!
# '''
# Now make a function that recreates formula (12) from Zhou 2007
# for a given user, get f'(o_j) for each object (restaurant)
f.prime <- function(graph, Wmat, user)
{
    # first get neighbors (objects) of user (restaurants that user likes)
    nbhd <- neighbors(graph=graph,v=user)
    # next, for a given object, get the sum of the similarities it has to the
    # restaurants that the user likes
    offset <- length(which(V(graph)$isuser %in% 1))
    # get indices of objects that we will search for in Wmat
    object.list <- which(V(graph)$type %in% 1)-offset
    # get indices of neighbors of W that we will look for
    nbhd.ind <- which(V(graph) %in% nbhd)-offset
    # for each object in Wmat, get sum of entries of all of the users in that neighborood
    fp <- sapply(object.list, function(obj) {return(sum(Wmat[obj,nbhd.ind]))})
    # get names of objects
    vs <- which(V(graph)$type %in% 1)
    names(fp) <- names(V(graph)[vs])
    fp[names(fp) %in% names(neighbors(graph=graph,v=user))] <- -1
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
    s.numerator <- length(which(n1 %in% n2))
    s.denominator <- min(length(n1), length(n2))
    return(s.numerator/s.denominator)
}

# also from Zhou 2007
cf.predicted.score <- function(graph, user, object, S)
{
    numerator <- 0
    denominator <- 0
    if (get.edge.ids(graph,c(user,object)) == 0)
    {
        user.list <- V(graph)[V(graph)$isuser]
        # similarity_list <- sapply(user.list, similarity.function, user_j=user, graph=graph)
        similarity_list <- S[user,]
        user.neighbors <- which(user.list %in% neighbors(graph=graph, v=V(graph)[object]))
        user.neighbors <- setdiff(user.neighbors,user)
        numerator <- sum(unlist(similarity_list[user.neighbors]))
        denominator <- sum(unlist(similarity_list))
        return(numerator/denominator)
    }
    else
    {
        print('poop')
        # get real recommended score if the user has rated the restaurant
        return(-1)
    }
}

# Following function is for collaborative filtering. Need similarity matrix between nodes!!!
generate.similarityMatrix <- function(gr, outfile)
{
    # get list of users
    user.list <- V(gr)[V(gr)$isuser]
    # initialize similarity matrix
    S <- Matrix(0,length(user.list), length(user.list))
    # for each user
    for (i in c(1:length(user.list)))
    {
        pth <- proc.time()
        # find the similarity of each user to user_i
        S[i,] <- sapply(user.list, similarity.function, user_j=user.list[i], graph=gr)
        print(proc.time()-pth)
    }
    save(S, file=outfile)
}

collaborative.filter <- function(user, train.graph, similarity_matrix, threshold)
{
    # set get handle to function that determines user's predicted score for a restaurant
    cf.ps.handle <- Curry(cf.predicted.score, graph=train.graph, user=user, S=similarity_matrix)
    # get indices of restaurants in training graph
    restaurant.indices <- which(V(train.graph)$type %in% 1) 
    # for this user, get recommendations for all restaurants
    ratings.list <- mclapply(restaurant.indices, cf.ps.handle, mc.preschedule=TRUE, mc.cores=detectCores())
    
    businesses <- names(V(train.graph)[restaurant.indices])
    
    names(ratings.list) <- businesses
    
    ratings.list <- outarray
    
    save(ratings.list, file=paste('cf_ratings/cf_ratings_user',toString(user),'_', toString(threshold),'.RData', sep=''))
}

makeAndSaveW <- function(gr, outfilename='object_projection_train.RData')
{
    vs <- which(V(gr)$type %in% 1)
    W <- get.W(vs, gr)
    save(W,file=outfilename)
}