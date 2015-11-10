# Figure 0 -> example of user and his restaurants

source('init.r')
source('utils.r')
gr <- init.graph()
sg<-induced.subgraph(graph=gr, vids=match(neighborhood(gr, order=1, nodes=100)[[1]], V(gr)))
V(sg)[1]$name <- "Gabi"
V(sg)[2]$name <- "Alexander Michael's"
V(sg)[3]$name <- 'Ilios Noche'
V(sg)[4]$name <- 'Encore Bistro & Bar'
png(filename='figure0.png')
plot(sg,layout=layout.bipartite)
dev.off()
# Figure 1 -> strength of users (number of postive reviews given out by each user)
obj.str <- graph.strength(graph=gr, vids=V(gr)[V(gr)$isuser])
png(filename='figure1.png')
hist(obj.str,breaks=100, xlim=c(1,100), main='Strengths of each user node', xlab='Strength', ylab='Count')
dev.off()
# Figure 2
load('review_edges_ds.RData')
obj.str <- graph.strength(graph=gr, vids=V(gr)[V(gr)$isuser])
png(filename='figure2.png')
hist(obj.str,breaks=100, xlim=c(1,100), main='Strengths of each user node', xlab='Strength', ylab='Count')
dev.off()
# Figure 4

# Figure 5

# make a figure as follows for testing algos (here we measure the hitting rate):
library(ggplot2)
library(reshape)

load(file='resultsdataframe.RData')
png(filename='figure4.png')
 ggplot(data=results.df, aes(x = L)) +
 geom_line(aes(y = nbimean, color='red')) +
 geom_line(aes(y = cfmean, color='blue')) + 
 labs(title='Hitting rates for NBI and CF', x='Ranking lengths', y='Hitting rate')+
 scale_color_manual('Ranking method', labels=c('NBI','CF'), values=c('red','blue'))
 dev.off()
 # http://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot

 # Next, add count/L instead of other.....

# Add coverage if have time
# 95% CI?

# figure out: if add noise, how stable will results be?
