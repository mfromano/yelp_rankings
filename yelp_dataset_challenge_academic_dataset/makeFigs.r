# Figure 0 -> example of user and his restaurants

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

load('review_edges_ds0.RData')
# Figure 1 -> strength of users (number of postive reviews given out by each user)
obj.str <- graph.strength(graph=gr, vids=V(gr)[V(gr)$isuser])
png(filename='figure1.png')
hist(obj.str,breaks=100, xlim=c(1,100), main='Degree distribution of each user node', xlab='Degree', ylab='Count')
dev.off()
# Figure 2
load('review_edges_ds9.RData')
obj.str <- graph.strength(graph=gr, vids=V(gr)[V(gr)$isuser])
png(filename='figure2.png')
hist(obj.str,breaks=100, xlim=c(1,100), main='Degree distribution of each user node', xlab='Degree', ylab='Count')
dev.off()
# Figure 4

# FIGURE 1
source('subgraph_bias.r')

# Figure 5

# make a figure as follows for testing algos (here we measure the hitting rate):


load(file='resultsdataframe4.RData')
# png(filename='figure4.png')
p1 <- ggplot(data=results.df, aes(x = L)) +
 geom_line(aes(x = L, y = nbimean, color='red')) +
 geom_line(aes(x = L, y = cfmean, color='blue')) + 
 labs(title='Threshold of 5', x='Recommendation lengths', y='Hitting rate')+
 scale_color_manual('Method', labels=c('CF','NBI'), values=c('blue','red'))
 # dev.off()

load(file='resultsdataframe9.RData')
# png(filename='figure5.png')
p2 <- ggplot(data=results.df, aes(x = L)) +
 geom_line(aes(x = L, y = nbimean, color='red')) +
 geom_line(aes(x = L, y = cfmean, color='blue')) + 
 labs(title='Threshold of 10', x='Recommendation lengths', y='Hitting rate')+
 scale_color_manual('Method', labels=c('CF','NBI'), values=c('blue','red'))
 # dev.off()
 # http://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot

load(file='resultsdataframe14.RData')
# png(filename='figure6.png')
p3 <- ggplot(data=results.df, aes(x = L)) +
 geom_line(aes(x = L, y = nbimean, color='red')) +
 geom_line(aes(x = L, y = cfmean, color='blue')) + 
 labs(title='Threshold of 15', x='Recommendation lengths', y='Hitting rate') +
 scale_color_manual('Method', labels=c('CF','NBI'), values=c('blue','red'))
 # dev.off()

load(file='resultsdataframe24.RData')
# png(filename='figure7.png')
p4 <- ggplot(data=results.df, aes(x = L)) +
 geom_line(aes(x = L, y = nbimean, color='red')) +
 geom_line(aes(x = L, y = cfmean, color='blue')) + 
 labs(title='Threshold of 25', x='Recommendation lengths', y='Hitting rate')+
 scale_color_manual('Method', labels=c('CF','NBI'), values=c('blue','red'))
 # dev.off()
 # Next, add count/L instead of other.....
 png(filename='figure4.png')
grid_arrange_shared_legend(p1, p2, p3, p4)
dev.off()
# Add coverage if have time
# 95% CI?

# First, compare across different thresholds
compare.recommenders <- function(recommender.system='nbi')
{
    load('resultsdataframe24.RData')
    results.df.24 <- results.df
    results.df.24$L <- results.df.24$L / 1448

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
        'nbi'=return(ggplot() +
            geom_line(data=results.df.4, aes(x=L, y=nbimean, color='green')) +
            geom_line(data=results.df.9, aes(x=L, y=nbimean, color='blue')) +
            geom_line(data=results.df.14, aes(x=L, y=nbimean, color='red')) +
            geom_line(data=results.df.24, aes(x=L, y=nbimean, color='orange')) +
            labs(title='Hitting rates for NBI compared with different thresholds', x='Proportion of Recommendation list', y='Hitting rate') +
            scale_color_manual('Recommendation method', labels=c('Threshold=5','Threshold=10','Threshold=15', 'Threshold=25'), values=c('green','blue','red', 'orange'))),
        'cf'=return(ggplot() +
            geom_line(data=results.df.4, aes(x=L, y=cfmean, color='green')) +
            geom_line(data=results.df.9, aes(x=L, y=cfmean, color='blue')) +
            geom_line(data=results.df.14, aes(x=L, y=cfmean, color='red')) +
            geom_line(data=results.df.24, aes(x=L, y=cfmean, color='orange')) +
            labs(title='Hitting rates for CF compared with different thresholds', x='Proportion of Recommendation list', y='Hitting rate') +
            scale_color_manual('Recommendation method', labels=c('Threshold=5','Threshold=10','Threshold=15', 'Threshold=25'), values=c('green','blue','red', 'orange')))
    )
}

png(filename='figure8.png')
p1 <- compare.recommenders('nbi')

p2 <- compare.recommenders('cf')
grid_arrange_shared_legend(p1, p2)
dev.off()