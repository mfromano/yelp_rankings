graph.threshold <-
function(gr, threshold)
{
    vids <- V(gr)[graph.strength(gr) > threshold]
    vids <- vids[vids$type == FALSE]
    return(induced.subgraph(gr,vids=unique(unlist(neighborhood(gr,order=1,nodes=vids)))))
}
