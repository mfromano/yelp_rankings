source('utils.r')
source('init.r')

coverage <- function()
{
    load('review_edges_ds.RData')
    users <- which(V(gr)$isuser %in% 1)
    load('object_projection.RData')
    fp <- sapply(users, f.prime, graph=gr, Wmat=W)
    return(fp)
}

load('review_edges_ds.RData')
vs <- which(V(gr)$type %in% 1)
W <- get.W(vs, gr)
save(W,file='object_projection.RData')
fp <- coverage()
save(fp, 'fprime_all.RData')