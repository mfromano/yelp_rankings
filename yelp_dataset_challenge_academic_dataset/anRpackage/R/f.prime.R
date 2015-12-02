f.prime <-
function(graph, Wmat, user)
{
    # first get neighbors (objects) of user (restaurants that user likes)
    nbhd <- neighbors(graph=graph,v=user)
    # next, for a given object, get the sum of the similarities it has to the
    # restaurants that the user likes
    offset <- length(which(V(graph)$isuser %in% 1))
    # get indices of objects that we will search for in Wmat
    object.list <- which(V(graph)$type %in% 1)-offset
    # get indices of neighbors of W that we will look for
    nbhd.ind <- which(V(graph) %in% nbhd)-offset
    # for each object in Wmat, get sum of entries of all of the users in that neighborood
    fp <- sapply(object.list, function(obj) {return(sum(Wmat[obj,nbhd.ind]))})
    # get names of objects
    vs <- which(V(graph)$type %in% 1)
    names(fp) <- names(V(graph)[vs])
    fp <- fp[!(names(fp) %in% names(neighbors(graph=graph,v=user)))]
    fp <- sort(fp, decreasing=TRUE)
    return(fp)
}
