get.W <-
function(vids, graph)
{
    W <- Matrix(0,length(vids),length(vids))
    for (i in c(1:length(vids))) 
    {
        pth <- proc.time()
        W[i,] <- unlist(mclapply(vids, w.ij, graph=graph, i=vids[i], mc.preschedule=TRUE, mc.cores=detectCores()))
        # W[i,] <- unlist(lapply(vids, w.ij, graph=graph, i=i))   
        print(proc.time() - pth)
    }
    return(W)
}
