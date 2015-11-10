source('init.r')
source('utils.r')

hitting.rate <- function(graph.train, graph.probe, user.id, L)
{   
    # Get f prime for particular user using test data
    load('similarity_matrix.RData')
    vs <- which(V(graph.train)$type %in% 1)
    user <- match(user.id, names(V(graph.train)))
    load(paste('cf_ratings/cf_ratings_user',toString(user),'.RData', sep=''))
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


get.hitting.rate.cf <- function(probe.graph, train.graph, outfile='hittingRateCF.RData')
{
    users.in.probe <- length(V(probe.graph)[V(probe.graph)$isuser])
    L <- seq(from=1,to=users.in.probe, by=20)
    x <- Matrix(0,users.in.probe,length(L))

    for (i in c(1:users.in.probe))
    {
        pth <- proc.time()
        user.id <- names(V(probe.graph)[V(probe.graph)$isuser])[i]
        x[i,] <- unlist(mclapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, user.id=user.id, mc.preschedule=TRUE, mc.cores=detectCores()))
        # x[i,] <- sapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, user.id=user.id)
        print(proc.time() - pth)
    }
    save(x, outfile)
}

load('review_edges_train.RData')
generate.similarityMatrix(gr=train.graph, outfile='similarity_matrix_train.RData')

load('review_edges_probe.RData')
get.hitting.rate.cf(train.graph, probe.graph)

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