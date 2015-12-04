source('utils.r')

# for each entry in probe
#     if oj is in ui's recommendation list
#       ui-oj is a "hit"
#       
hitting.rate <- function(graph.train, graph.probe, user.id, L, fp)
{   
    # Get f prime for particular user using test data
    vs <- which(V(graph.train)$type %in% 1)

    recommend.list <- fp[1:L]

    # get neighborhood of particular user in probe graph
    user.probe <- match(user.id, names(V(graph.probe)))
    nbhd <- neighbors(graph=graph.probe, v=user.probe)

    # nbhd <- V(graph.train)[intersect(V(graph.train), nbhd)]

    # compute hitting rate
    hr <- length(which(names(nbhd) %in% names(recommend.list)))/length(names(nbhd))
    
    stopifnot(length(which(names(nbhd) %in% names(fp)))/length(names(nbhd)) >= 1)

    if (hr > 1)
    {
        hr <- 1
    }
    return(hr)
}

test.hitting.rate.nbi <- function(W, probe.graph, train.graph)
{
    users.in.probe <- length(V(probe.graph)[V(probe.graph)$isuser])
    restaurants.in.probe <- length(V(train.graph)[!V(train.graph)$isuser])
    L <- restaurants.in.probe
    x <- Matrix(0,users.in.probe,length(L))

    for (user in c(1:users.in.probe))
    {
        user.id <- names(V(probe.graph)[V(probe.graph)$isuser])[user]
        user.probe <- match(user.id, names(V(probe.graph)))
        nbhd <- neighbors(graph=probe.graph, v=user.probe)
        fp <- f.prime(graph=train.graph, Wmat=W, user=user)
        stopifnot(length(which(names(nbhd) %in% names(fp)))/length(names(nbhd)) >= 1)
        print(intersect(names(nbhd), names(fp)))
        print(names(nbhd))
        stopifnot(length(intersect(names(nbhd), names(fp))) / length(names(nbhd)) >= 1)
    }
    print('success!!')
}

# W should be for the training graph, not for the probe graph!
get.hitting.rate.nbi <- function(W, probe.graph, train.graph, outfilename='hittingRateNBI.RData')
{
    num.probes <- length(V(probe.graph)[V(probe.graph)$isuser])
    num.restaurants <- length(V(train.graph)[!V(train.graph)$isuser])
    L <- seq(from=1,to=num.restaurants, by=20)
    x <- Matrix(0,num.probes,length(L))
    for (i in c(1:num.probes))
    {
        user.id <- names(V(probe.graph)[V(probe.graph)$isuser])[i]
        user <- match(user.id, names(V(train.graph)))
        print(user)
        fp <- f.prime(graph=train.graph, Wmat=W, user=user)
        x[i,] <- unlist(mclapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, fp=fp, user.id=user.id, mc.preschedule=TRUE, mc.cores=detectCores()))
        # x[i,] <- unlist(lapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, user.id=user.id, W=W))
    }
    save(x, file=outfilename)
}

run.nbi <- function(threshold=10)
{
    load(paste('review_edges_train', toString(threshold), '.RData', sep=''))
    # makeAndSaveW(train.graph, outfilename=paste('object_projection_train', toString(threshold), '.RData', sep=''))
    load(paste('object_projection_train', toString(threshold), '.RData', sep=''))
    load(paste('review_edges_probe', toString(threshold), '.RData', sep=''))
    load(paste('review_edges_train', toString(threshold), '.RData', sep=''))
    get.hitting.rate.nbi(W=W, probe.graph=probe.graph, train.graph=train.graph, outfilename=paste('hittingRateNBI',toString(threshold),'.RData', sep=''))
}