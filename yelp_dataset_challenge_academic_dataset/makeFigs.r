# Figure 1 -> strength of restaurants (number of positive reviews for restaurant)
source('init.r')
source('utils.r')
gr <- init.graph()
obj.str <- graph.strength(graph=gr, vids=V(gr)[!V(gr)$isuser])
hist(obj.str,breaks=100, main='Strengths of each restaurant', xlab='Strength', ylab='Count')

# Figure 2 -> strength of users (number of postive reviews given out by each user)
obj.str <- graph.strength(graph=gr, vids=V(gr)[V(gr)$isuser])
hist(obj.str,breaks=100, main='Strengths of each user', xlab='Strength', ylab='Count')

# Figure 3

# Figure 4

# Figure 5

# make a figure as follows for testing algos (here we measure the hitting rate):
library(ggplot2)
library(reshape)
load('review_edges_train.RData')
load('review_edges_probe.RData')
num.probes <- length(V(probe.graph)[V(probe.graph)$isuser])
Lvals <- seq(from=1,to=num.probes, by=20)

load('hittingRateNBI.RData')
mns <- apply(x, 2, mean)
errs <- 2*sqrt(apply(x, 2, var))

results.df <- data.frame(
    nbimean=mns,
    nbiserr=errs,
    L=Lvals
    )

load('hittingRateCF.RData')
mns <- apply(x, 2, mean)
errs <- 2*sqrt(apply(x, 2, var))
results.df$cfmean <- mns
results.df$cfserr <- errs
results.df$L <- L

save(results.df, file='resultsdataframe.RData')
png(filename='hittingrates.png')
 ggplot(data=results.df, aes(x = L)) +
 geom_line(aes(y = nbimean, color='red')) +
 geom_line(aes(y = cfmean, color='blue')) + 
 labs(title='Hitting rates for NBI and CF', x='Ranking lengths', y='Hitting rate')+
 scale_color_manual('Ranking method', labels=c('NBI','CF'), values=c('red','blue'))
 dev.off()
 # http://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot