source('init.r')
source('utils.r')

user <- 1

load('review_edges_train.RData')
vs <- which(V(train.graph)$type %in% 1)
W <- get.W(vs, train.graph)
save(W,file='object_projection_train.RData')
print('object projection saved!')

load('object_projection_train.RData')
fp <- f.prime(graph=train.graph, Wmat=W, user=user)
write.csv(fp,file='nbi_ratings_train.csv')