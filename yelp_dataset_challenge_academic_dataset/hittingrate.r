# hittingrate.r

# for each entry in probe
#     if oj is in ui's recommendation list
#       ui-oj is a "hit"
#       
hitting.rate <- function(graph.train, graph.probe, user.id, L)
{   
    # Get f prime for particular user using test data
    load('object_projection_train.RData')
    vs <- which(V(graph.train)$type %in% 1)
    user <- match(user.id, names(V(graph.train)))
    fp <- f.prime(graph=graph.train, Wmat=W, user=user)
    recommend.list <- fp[1:L]

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


source('init.r')
source('utils.r')

# GET W FOR TRAINING DATA
load('review_edges_train.RData')
vs <- which(V(train.graph)$type %in% 1)
# W <- get.W(vs, train.graph)
# save(W,file='object_projection_train.RData')
# GET THE GRAPH FOR 
load('review_edges_probe.RData')
# TODO: construct W for training data, then run 
# length(names(V(graph.probe))) == 422
num.probes <- length(V(probe.graph)[V(probe.graph)$isuser])

L <- seq(from=1,to=num.probes, by=20)

x <- Matrix(0,num.probes,length(L))

for (i in c(1:num.probes))
{
    pth <- proc.time()
    user.id <- names(V(probe.graph)[V(probe.graph)$isuser])[i]
    x[i,] <- unlist(mclapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, user.id=user.id, mc.preschedule=TRUE, mc.cores=detectCores()))
    print(proc.time() - pth)
}
save(x, file='hittingRateNBI.RData')
# 
