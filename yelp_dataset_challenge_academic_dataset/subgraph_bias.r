# the following script contains functions that
# will help to elucidate the bias in subgraphs
source('utils.r')
library(ggplot2)
library(reshape)

logLogPlot <- function(threshold0=0, threshold1=4, threshold5=9, threshold2=14, threshold3=24)
{
    load(paste('review_edges_ds', toString(threshold0), '.RData', sep=''))
    restaurant.strengths <- graph.strength(graph=gr, vids=which(V(gr)$isuser %in% 0))
    loghist0 <- hist(log(restaurant.strengths), breaks=20)

    load(paste('review_edges_ds', toString(threshold1), '.RData', sep=''))
    restaurant.strengths <- graph.strength(graph=gr, vids=which(V(gr)$isuser %in% 0))
    loghist <- hist(log(restaurant.strengths), breaks=loghist0$breaks)

    load(paste('review_edges_ds', toString(threshold5), '.RData', sep=''))
    restaurant.strengths5 <- graph.strength(graph=gr, vids=which(V(gr)$isuser %in% 0))
    loghist5 <- hist(log(restaurant.strengths5), breaks=loghist$breaks)


    load(paste('review_edges_ds', toString(threshold2), '.RData', sep=''))
    restaurant.strengths2 <- graph.strength(graph=gr, vids=which(V(gr)$isuser %in% 0))
    loghist2 <- hist(log(restaurant.strengths2), breaks=loghist$breaks)

    load(paste('review_edges_ds', toString(threshold3), '.RData', sep=''))
    restaurant.strengths3 <- graph.strength(graph=gr, vids=which(V(gr)$isuser %in% 0))
    loghist3 <- hist(log(restaurant.strengths3), breaks=loghist$breaks)

    rest.df.hist <- data.frame(
            strengths.0=loghist0$density,
            strengths.5=loghist$density,
            strengths.10=loghist5$density,
            strengths.15=loghist2$density,
            strengths.25=loghist3$density,
            centers=loghist$mids,
            breaks=loghist$breaks[2:length(loghist$breaks)]
        )
    return(rest.df.hist)
}

rest.df.hist <- logLogPlot()
mdata <- melt(rest.df.hist, id=c("centers", "breaks"))

png(filename='figure3.png')
ggplot(data=mdata, aes(x=centers, y=value, fill=variable))+
    geom_bar(stat='identity', position=position_dodge()) +
    ylab("Proportion of vertices") + xlab("Log(Degree)")+
    scale_fill_discrete(labels=c('Threshold=0','Threshold=5','Threshold=10', 'Threshold=15', 'Threshold=25'))+
    ggtitle('Log degree distributions using different thresholds')
dev.off()