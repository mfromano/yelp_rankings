split.graphs <-
function(threshold=10)
{
    load(file=paste('review_edges_ds', toString(threshold), '.RData', sep=''))
    rand.list <- sample(c(1:length(E(gr))), floor(length(E(gr))*.9), replace=FALSE)
    train.list <- E(gr)[rand.list]
    probe.list <- setdiff(E(gr),train.list)
    train.graph <- subgraph.edges(graph=gr, eids=train.list)
    probe.graph <- subgraph.edges(graph=gr, eids=probe.list)
    save(train.graph, file=paste('review_edges_train',toString(threshold),'.RData', sep=''))
    save(probe.graph, file=paste('review_edges_probe',toString(threshold),'.RData', sep=''))
}
