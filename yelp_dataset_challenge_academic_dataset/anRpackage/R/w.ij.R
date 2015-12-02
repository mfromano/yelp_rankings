w.ij <-
function(graph, i, j)
{
    k.xj <- graph.strength(graph,vids=j)[[1]]

    objects.in.common <- intersect(neighborhood(graph=graph,order=1,nodes=i)[[1]],neighborhood(graph=graph,order=1,nodes=j)[[1]])
    # for each object
    if (length(objects.in.common) > 0)
    {
        total <- sum(1/graph.strength(graph=graph,vids=objects.in.common))
        return(total/k.xj)
    }
    else
    {
        return(0)
    }
}
