
# How to use this file:
# First, get the similarity matrix using generate.similarityMatrix()
# Next, use the similarity matrix to use collaborative filtering using:
#       collaborative.filter(user, train.graph, similarity.matrix)
# Finally, look to see how well the collaborative filter performed by computing the hitting rate
# 
# Example code:
# ############################################
# load('review_edges_train.RData')
# 
# generate.similarityMatrix(gr=train.graph, outfile='similarity_matrix_train10.RData')
# 
# load('similarity_matrix_train10.RData')
# 
# sapply(X=which(V(gr)$isuser %in% 1), FUN=collaborative.filter, train.graph=train.graph, similarity_matrix=similarity.matrix)
# 
# load('review_edges_probe.RData')
# 
# get.hitting.rate.cf(probe.graph, train.graph, similarity_matrix='similarity_matrix_train.RData', outfile='hittingRateCF.RData')
# #############################################

source('utils.r')

hitting.rate <- function(graph.train, graph.probe, user.id, similarity.matrix, L, threshold)
{   

    user <- match(user.id, names(V(graph.train)))
    load(paste('cf_ratings/cf_ratings_user',toString(user), '_', toString(threshold),'.RData', sep=''))
    ratings.list <- sort(unlist(ratings.list), decreasing=TRUE)
    recommend.list <- ratings.list[1:L]

    # get neighborhood of particular user in probe graph
    user.probe <- match(user.id, names(V(graph.probe)))
    nbhd <- neighbors(graph=graph.probe, v=user.probe)
    
    # compute hitting rate
    hr <- length(which(names(nbhd) %in% names(recommend.list)))/length(names(nbhd))

    stopifnot(length(which(names(nbhd) %in% names(ratings.list)))/length(names(nbhd)) >= 1)
    if (hr > 1)
    {
        print('shit')
        hr <- 1
    }
    return(hr)
}

test.hitting.rate.cf <- function(probe.graph, train.graph, similarity.matrix, threshold)
{
    users.in.probe <- length(V(probe.graph)[V(probe.graph)$isuser])
    restaurants.in.probe <- length(V(train.graph)[!V(train.graph)$isuser])
    L <- restaurants.in.probe
    x <- Matrix(0,users.in.probe,length(L))

    for (user in c(1:users.in.probe))
    {
        user.id <- names(V(probe.graph)[V(probe.graph)$isuser])[user]
        # x[i,] <- unlist(mclapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, similarity.matrix=similarity.matrix, user.id=user.id, threshold=threshold, mc.preschedule=TRUE, mc.cores=detectCores()))
        load(paste('cf_ratings/cf_ratings_user',toString(user), '_', toString(threshold),'.RData', sep=''))
        user.probe <- match(user.id, names(V(probe.graph)))
        nbhd <- neighbors(graph=probe.graph, v=user.probe)
        stopifnot(length(which(names(nbhd) %in% names(ratings.list)))/length(names(nbhd)) >= 1)
    }
    print('success!!')
}

get.hitting.rate.cf <- function(probe.graph, train.graph, similarity.matrix, threshold, outfile='hittingRateCF.RData')
{
    users.in.probe <- length(V(probe.graph)[V(probe.graph)$isuser])
    restaurants.in.probe <- length(V(train.graph)[!V(train.graph)$isuser])
    L <- seq(from=1,to=restaurants.in.probe, by=20)
    x <- Matrix(0,users.in.probe,length(L))

    for (i in c(1:users.in.probe))
    {
        user.id <- names(V(probe.graph)[V(probe.graph)$isuser])[i]
        x[i,] <- unlist(mclapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, similarity.matrix=similarity.matrix, user.id=user.id, threshold=threshold, mc.preschedule=TRUE, mc.cores=detectCores()))
        # x[i,] <- sapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, user.id=user.id, similarity.matrix=similarity.matrix, threshold=threshold)
    }
    save(x, file=outfile)
}

run.collaborative.filter <- function(threshold=0)
{
    # downsample.graph(threshold)
    # split.graphs(threshold)
    load(paste('review_edges_train', toString(threshold),'.RData', sep=''))
    # generate.similarityMatrix(gr=train.graph, outfile=paste('similarity_matrix_train',toString(threshold), '.RData', sep=''))
    load(paste('similarity_matrix_train',toString(threshold), '.RData', sep=''))
    # sapply(X=which(V(train.graph)$isuser %in% 1), FUN=collaborative.filter, train.graph=train.graph, similarity_matrix=S, threshold=threshold)
    load(paste('review_edges_probe', toString(threshold),'.RData', sep=''))
    get.hitting.rate.cf(probe.graph, train.graph, similarity.matrix=S, threshold=threshold, outfile=paste('hittingRateCF',toString(threshold),'.RData', sep=''))
}

# load('review_edges_train.RData')
# generate.similarityMatrix(gr=train.graph, outfile='similarity_matrix_train.RData')

# load('similarity_matrix_train.RData')

# load('review_edges_probe.RData')
# get.hitting.rate.cf(probe.graph, train.graph, similarity_matrix='similarity_matrix_train.RData', outfile='hittingRateCF.RData')

# library(ggplot2)
# load('review_edges_train.RData')
# load('review_edges_probe.RData')
# num.probes <- length(V(probe.graph)[V(probe.graph)$isuser])
# L <- seq(from=1,to=num.probes, by=20)
# load('hittingRateCF.RData')
# df <- as.data.frame(data.matrix(x))
# mns <- apply(x, 2, mean)
# errs <- 2*sqrt(apply(x, 2, var))
# qplot(L, mns) # +