source('init.r')
source('utils.r')

collaborative.filter <- function(user)
{
    load('review_edges_train.RData')
    cf.ps.handle <- Curry(cf.predicted.score, graph=train.graph, user=user)
    restaurant.indices <- which(V(train.graph)$type %in% 1) # gets indices of restaurants
    pth <- proc.time()
    outarray <- mclapply(restaurant.indices, cf.ps.handle, mc.preschedule=TRUE, mc.cores=detectCores())
    print(proc.time()-pth)
    businesses <- names(V(train.graph)[restaurant.indices])
    names(outarray) <- businesses
    ratings.list <- outarray
    # ratings.list <- sort(ratings.list, decreasing=TRUE)
    save(ratings.list, file=paste('cf_ratings/cf_ratings_user',toString(user),'.RData'))
    # write.csv(ratings.list,file=paste('cf_ratings/cf_ratings_user',toString(user),'.csv'))
}

# create similarity matrix

load('review_edges_train.RData')
user.list <- V(train.graph)[V(train.graph)$isuser]
S <- Matrix(0,length(user.list), length(user.list))
for (i in c(1:length(user.list)))
{
    pth <- proc.time()
    S[i,] <- sapply(user.list, similarity.function, user_j=user.list[i], graph=train.graph)
    print(proc.time()-pth)
}
save(S, file='similarity_matrix.RData')
    
# copy and paste the next few lines to....
hitting.rate <- function(graph.train, graph.probe, user.id, L)
{   
    # Get f prime for particular user using test data
    load('similarity_matrix.RData')
    vs <- which(V(graph.train)$type %in% 1)
    user <- match(user.id, names(V(graph.train)))
    load(paste('cf_ratings/cf_ratings_user',toString(user),'.RData'))
    ratings.list <- sort(unlist(ratings.list), decreasing=TRUE)
    recommend.list <- ratings.list[1:L]

    # get neighborhood of particular user in probe graph
    user.probe <- match(user.id, names(V(graph.probe)))
    nbhd <- neighbors(graph=graph.probe, v=user.probe)

    # compute hitting rate
    hr <- length(intersect(names(recommend.list), names(nbhd)))/length(nbhd)
    if (hr > 1)
    {
        hr <- 1
    }
    return(hr)
}

load('review_edges_train.RData')
vs <- which(V(train.graph)$type %in% 1)
load('review_edges_probe.RData')

num.probes <- length(V(probe.graph)[V(probe.graph)$isuser])

L <- seq(from=1,to=num.probes, by=20)

x <- Matrix(0,num.probes,length(L))

for (i in c(1:num.probes))
{
    pth <- proc.time()
    user.id <- names(V(probe.graph)[V(probe.graph)$isuser])[i]
    # x[i,] <- unlist(mclapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, user.id=user.id, mc.preschedule=TRUE, mc.cores=detectCores()))
    x[i,] <- sapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, user.id=user.id)

    print(proc.time() - pth)
}
save(x, file='hittingRateCF.RData')

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