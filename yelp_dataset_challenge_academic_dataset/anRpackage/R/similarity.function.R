similarity.function <-
function(user_i, user_j, graph)
{
    n1 <- neighbors(graph=graph, v=user_i)
    n2 <- neighbors(graph=graph, v=user_j)
    s.numerator <- length(intersect(n1,n2))
    s.denominator <- min(length(n1), length(n2))
    return(s.numerator/s.denominator)
}
