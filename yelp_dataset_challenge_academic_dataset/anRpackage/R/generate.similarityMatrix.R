generate.similarityMatrix <-
function(gr, outfile)
{
    # get list of users
    user.list <- V(gr)[V(gr)$isuser]
    # initialize similarity matrix
    S <- Matrix(0,length(user.list), length(user.list))
    # for each user
    for (i in c(1:length(user.list)))
    {
        pth <- proc.time()
        # find the similarity of each user to user_i
        S[i,] <- sapply(user.list, similarity.function, user_j=user.list[i], graph=gr)
        print(proc.time()-pth)
    }
    save(S, file=outfile)
}
