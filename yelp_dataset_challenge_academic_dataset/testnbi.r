source('nbi.r')
threshold=10
load(paste('review_edges_train', toString(threshold), '.RData', sep=''))
# makeAndSaveW(train.graph, outfilename=paste('object_projection_train', toString(threshold), '.RData', sep=''))
load(paste('object_projection_train', toString(threshold), '.RData', sep=''))
load(paste('review_edges_probe', toString(threshold), '.RData', sep=''))
load(paste('review_edges_train', toString(threshold), '.RData', sep=''))
num.probes <- length(V(probe.graph)[V(probe.graph)$isuser])
num.restaurants <- length(V(train.graph)[!V(train.graph)$isuser])
L <- c(1, num.restaurants)
x <- Matrix(0,num.probes,length(L))
for (i in c(1:num.probes))
    {
        user.id <- names(V(probe.graph)[V(probe.graph)$isuser])[i]
        user <- match(user.id, names(V(train.graph)))
        fp <- f.prime(graph=train.graph, Wmat=W, user=user)
        x[i,] <- unlist(mclapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, fp=fp, user.id=user.id, mc.preschedule=TRUE, mc.cores=detectCores()))
        # x[i,] <- unlist(lapply(X=L, FUN=hitting.rate, graph.probe=probe.graph, graph.train=train.graph, user.id=user.id, W=W))
    }
save(x, file='test.nbi.RData')