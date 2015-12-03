source('init.r')
source('utils.r')

# for each entry in probe
#     if oj is in ui's recommendation list
#       ui-oj is a "hit"
#       
hitting.rate <- function(graph.train, graph.probe, user.id, L, W)
{   
    # Get f prime for particular user using test data
    vs <- which(V(graph.train)$type %in% 1)
    user <- match(user.id, names(V(graph.train)))
    fp <- f.prime(graph=graph.train, Wmat=W, user=user)
    recommend.list <- fp[1:L]

    # get neighborhood of particular user in probe graph
    user.probe <- match(user.id, names(V(graph.probe)))
    nbhd <- neighbors(graph=graph.probe, v=user.probe)

    # nbhd <- V(graph.train)[intersect(V(graph.train), nbhd)]

    # compute hitting rate
    hr <- length(intersect(names(recommend.list), names(nbhd)))/length(nbhd)
    

    if (hr > 1)
    {
        hr <- 1
    }
    return(hr)
}

# W should be for the training graph, not for the probe graph!
get.hitting.rate.nbi <- function(W, probe.graph, train.graph, outfilename='hittingRateNBI.RData')
{
    num.probes <- length(V(probe.graph)[V(probe.graph)$isuser])
    num.restaurants <- length(V(probe.graph)[!V(probe.graph)$isuser])
    L <- seq(from=1,to=num.restaurants, by=20)
    x <- Matrix(0,num.probes,length(L))
    for (i in c(1:num.probes))
    {
        pth <- proc.time()
        user.id <- names(V(probe.graph)[V(probe.graph)$isuser])[i]
        x[i,] <- unlist(mclapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, W=W, user.id=user.id, mc.preschedule=TRUE, mc.cores=detectCores()))
        # x[i,] <- unlist(lapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, user.id=user.id, W=W))
        print(proc.time() - pth)
    }
    save(x, file=outfilename)
}

run.nbi <- function(threshold=10)
{
    load(paste('review_edges_train', toString(threshold), '.RData', sep=''))
    makeAndSaveW(train.graph, outfilename=paste('object_projection_train', toString(threshold), '.RData', sep=''))
    load(paste('object_projection_train', toString(threshold), '.RData', sep=''))
    load(paste('review_edges_probe', toString(threshold), '.RData', sep=''))
    load(paste('review_edges_train', toString(threshold), '.RData', sep=''))
    get.hitting.rate.nbi(W=W, probe.graph=probe.graph, train.graph=train.graph, outfilename=paste('hittingRateNBI',toString(threshold),'.RData', sep=''))
}