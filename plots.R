library(ggplot2)

datapath <- '/Users/echellwig/Drive/OtherPeople/otterData/'
pp <- read.csv(file.path(datapath, 'vis/popplot.csv'))

####################################################

popplot <- ggplot(data=dfm) + geom_line(aes(x=Year, y=P_Otters), size=1.2)
popplot <- popplot + geom_ribbon(aes(x=Year, min=Lower95, max=Upper95), 
                                 alpha=0.3)
popplot <- popplot + geom_point(aes(x=Year, y=O_Otters), size=3)
popplot <- popplot + facet_wrap(~Site) + theme_bw(26)
popplot <- popplot + scale_y_continuous(breaks=seq(0, 15, by=5),
                                        limits=c(-8, 20))
popplot <- popplot + scale_x_continuous(breaks=seq(2013, 2021, by=2))
popplot <- popplot + labs(x='Year', y='Otter Population')
popplot <- popplot + geom_hline(yintercept=0, color='red2', size=1.3)

png(file.path(datapath, 'plots/population.png'), width=3000, height=2000, 
    res=150)
popplot
dev.off()
