# the following script contains functions that
# will help to elucidate the bias in subgraphs
source('init.r')
source('utils.r')
library(ggplot2)

logLogPlot <- function(threshold1=0, threshold2=10)
{
    load(paste('review_edges_ds', toString(threshold1), '.RData', sep=''))
    restaurant.strengths <- graph.strength(graph=gr, vids=which(V(gr)$isuser %in% 0))
    loghist <- hist(log(restaurant.strengths), breaks=30)

    load(paste('review_edges_ds', toString(threshold2), '.RData', sep=''))
    restaurant.strengths2 <- graph.strength(graph=gr, vids=which(V(gr)$isuser %in% 0))
    loghist2 <- hist(log(restaurant.strengths2), breaks=loghist$breaks)

    rest.df.hist <- data.frame(
            strengths=loghist$density,
            strengths2=loghist2$density,
            centers=loghist$mids,
            breaks=loghist$breaks[2:length(loghist$breaks)]
        )
    return(rest.df.hist)
}

mdata <- melt(rest.df.hist, id=c("centers", "breaks"))
ggplot(data=mdata, aes(x=centers, y=value, fill=variable))+ geom_bar(stat='identity', position=position_dodge()) + ylab("count") + xlab("Log(Degree)")