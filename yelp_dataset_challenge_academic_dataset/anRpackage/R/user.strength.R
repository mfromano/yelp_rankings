user.strength <-
function(gr, bins=100)
{
    vids <- V(gr)[V(gr)$type == FALSE]
    hist(vids,breaks=bins)
    return(graph.strength(gr, vids=vids))
}
