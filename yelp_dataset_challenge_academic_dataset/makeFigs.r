# Figure 0 -> example of user and his restaurants

source('init.r')
source('utils.r')
gr <- init.graph()
sg<-induced.subgraph(graph=gr, vids=match(neighborhood(gr, order=1, nodes=100)[[1]], V(gr)))
V(sg)[1]$name <- "User"
V(sg)[2]$name <- "Restaurant 1"
V(sg)[3]$name <- 'Restaurant 2'
V(sg)[4]$name <- 'Restaurant 3'
V(sg)$label.cex <- 2
png(filename='figure0.png')
plot(sg,layout=layout.bipartite, vertex.label.dist=1)
dev.off()


# Figure 1 -> strength of users (number of postive reviews given out by each user)
obj.str <- graph.strength(graph=gr, vids=V(gr)[V(gr)$isuser])
png(filename='figure1.png')
hist(obj.str,breaks=100, xlim=c(1,100), main='Degree distribution of each user node', xlab='Degree', ylab='Count')
dev.off()
# Figure 2
load('review_edges_ds.RData')
obj.str <- graph.strength(graph=gr, vids=V(gr)[V(gr)$isuser])
png(filename='figure2.png')
hist(obj.str,breaks=100, xlim=c(1,100), main='Degree distribution of each user node', xlab='Degree', ylab='Count')
dev.off()
# Figure 4

# Figure 5

# make a figure as follows for testing algos (here we measure the hitting rate):
library(ggplot2)
library(reshape)

load(file='resultsdataframe9.RData')
png(filename='figure4.png')
 ggplot(data=results.df, aes(x = L)) +
 geom_line(aes(x = L, y = nbimean, color='red')) +
 geom_line(aes(x = L, y = cfmean, color='blue')) + 
 labs(title='Hitting rates for NBI and CF', x='Ranking lengths', y='Hitting rate')+
 scale_color_manual('Ranking method', labels=c('CF','NBI'), values=c('blue','red'))
 dev.off()
 # http://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot

load(file='resultsdataframe14.RData')
png(filename='figure5.png')
 ggplot(data=results.df, aes(x = L)) +
 geom_line(aes(x = L, y = nbimean, color='red')) +
 geom_line(aes(x = L, y = cfmean, color='blue')) + 
 labs(title='Hitting rates for NBI and CF', x='Ranking lengths', y='Hitting rate')+
 scale_color_manual('Ranking method', labels=c('CF','NBI'), values=c('blue','red'))
 dev.off()

 # Next, add count/L instead of other.....

# Add coverage if have time
# 95% CI?

# First, compare across different thresholds
compare.nbi <- function(recommender.system='nbi')
{
    load('resultsdataframe14.RData')
    results.df.14 <- results.df
    results.df.14$L <- results.df.14$L / 1519

    load('resultsdataframe9.RData')
    results.df.9 <- results.df
    results.df.9$L <- results.df.9$L / 1540

    load('resultsdataframe4.RData')
    results.df.4 <- results.df
    results.df.4$L <- results.df.4$L / 1584

switch(recommender.system,
    'nbi'=ggplot() +
        geom_line(data=results.df.4, aes(x=L, y=nbimean, color='green')) +
        geom_line(data=results.df.9, aes(x=L, y=nbimean, color='blue')) +
        geom_line(data=results.df.14, aes(x=L, y=nbimean, color='red')) +
        labs(title='Hitting rates for NBI compared with different thresholds', x='Percent of ranking list', y='Hitting rate') +
        scale_color_manual('Ranking method', labels=c('Threshold=5','Threshold=10','Threshold=15'), values=c('green','blue','red')),
    'cf'=ggplot() +
        geom_line(data=results.df.4, aes(x=L, y=cfmean, color='green')) +
        geom_line(data=results.df.9, aes(x=L, y=cfmean, color='blue')) +
        geom_line(data=results.df.14, aes(x=L, y=cfmean, color='red')) +
        labs(title='Hitting rates for CF compared with different thresholds', x='Percent of ranking list', y='Hitting rate') +
        scale_color_manual('Ranking method', labels=c('Threshold=5','Threshold=10','Threshold=15'), values=c('green','blue','red'))
    )
}