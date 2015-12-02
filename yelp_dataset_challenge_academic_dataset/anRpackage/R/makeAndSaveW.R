makeAndSaveW <-
function(gr, outfilename='object_projection_train.RData')
{
    vs <- which(V(gr)$type %in% 1)
    W <- get.W(vs, gr)
    save(W,file=outfilename)
}
