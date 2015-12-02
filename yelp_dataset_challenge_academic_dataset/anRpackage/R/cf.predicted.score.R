cf.predicted.score <-
function(graph, user, object, S)
{
    numerator <- 0
    denominator <- 0
    if (get.edge.ids(graph,c(user,object)) == 0)
    {
        user.list <- V(graph)[V(graph)$isuser]
        # similarity_list <- sapply(user.list, similarity.function, user_j=user, graph=graph)
        similarity_list <- S[user,]
        user.neighbors <- intersect(user.list, neighbors(graph=graph, v=V(graph)[object]))
        user.neighbors <- setdiff(user.neighbors,user)
        numerator <- sum(unlist(similarity_list[user.neighbors]))
        denominator <- sum(unlist(similarity_list))
        return(numerator/denominator)
    }
    else
    {
        # get real recommended score if the user has rated the restaurant
        return(-1)
    }
}
