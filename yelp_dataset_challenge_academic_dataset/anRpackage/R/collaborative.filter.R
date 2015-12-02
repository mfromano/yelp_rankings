collaborative.filter <-
function(user, train.graph, similarity_matrix, threshold)
{
    cf.ps.handle <- Curry(cf.predicted.score, graph=train.graph, user=user, S=similarity_matrix)
    restaurant.indices <- which(V(train.graph)$type %in% 1) # gets indices of restaurants
    pth <- proc.time()
    outarray <- mclapply(restaurant.indices, cf.ps.handle, mc.preschedule=TRUE, mc.cores=detectCores())
    print(proc.time()-pth)
    businesses <- names(V(train.graph)[restaurant.indices])
    names(outarray) <- businesses
    ratings.list <- outarray
    save(ratings.list, file=paste('cf_ratings/cf_ratings_user',toString(user),'_', toString(threshold),'.RData', sep=''))
}
