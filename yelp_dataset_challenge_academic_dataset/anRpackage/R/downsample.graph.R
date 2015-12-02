downsample.graph <-
function(threshold=10)
{
    gr <- init.graph()
    gr <- graph.threshold(gr=gr, threshold=threshold)
    save(gr, file=paste('review_edges_ds', toString(threshold), '.RData', sep=''))
}
