library(igraph)
library(functional)
library(parallel)
library(Matrix)

init.graph <- function(file='review_edges.csv')
{
    a <- read.csv(file, header=F)
    gr <- graph.data.frame(a,directed=F)
    E(gr)$weight <- sapply(a$V3, as.numeric)
    # get the intersection of "users" and vertices, and set these to TRUE. Set others to false
    indices <- sapply(V(gr)$name, is.element, set=a$V1)
    # indices <- intersect(V(gr)$name, a$user)
    V(gr)[!indices]$type <- TRUE
    V(gr)[indices]$type <- FALSE
    V(gr)$isuser <- !V(gr)$type
    return(gr)
}

downsample.graph <- function(threshold=10)
{
    gr <- init.graph()
    gr <- graph.threshold(graph=gr, threshold=threshold)
    save(gr, file=paste('review_edges_ds', toString(threshold), '.RData', sep=''))
}

split.graphs <- function(threshold=10)
{
    load(file=paste('review_edges_ds', toString(threshold), '.RData', sep=''))
    rand.list <- sample(c(1:length(E(gr))), floor(length(E(gr))*.9), replace=FALSE)
    train.list <- E(gr)[rand.list]
    probe.list <- setdiff(E(gr),train.list)
    train.graph <- subgraph.edges(graph=gr, eids=train.list)
    probe.graph <- subgraph.edges(graph=gr, eids=probe.list)
    save(train.graph, file=paste('review_edges_train',toString(threshold),'.RData', sep=''))
    save(probe.graph, file=paste('review_edges_probe',toString(threshold),'.RData', sep=''))
}