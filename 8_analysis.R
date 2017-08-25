datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(rstan)
library(ggplot2)
otr <- read.csv(file.path(datapath, 'otterclean.csv'))
vl1 <- readRDS(file.path(datapath, 'models/varying1locationpost.RDS'))
vl2 <- readRDS(file.path(datapath, 'models/varying2locationpost.RDS'))


source('functions.R')

locs <- levels(factor(otr$location))

###########################################################################
otr2 <- otr[,c('location','year','pop')]
names(otr2) <- c('Site','YearID','O_Otters')

###########################################################################

##slope analysis

b1fixed <- extractpar(vl2, 'beta', location=NA, rows=2)

b1samples <- data.frame(sapply(1:14, function(loc) {
    extractpar(vl2, 'u', location=loc, rows=2) + b1fixed
}))
names(b1samples) <- locs

b1quants <- t(round(apply(b1samples, 2, quantile, probs=c(0.025, .10, .25,
                                                          .5, .75,.9, .975)),3))


locpredict <- sapply(1:14, function(i) {
    locpost(vl2, i, 'u', response='function')
})

pdat <- data.frame(loc=locs,
                   declineP=sapply(1:14, function(i) {
                       mean(b1samples[,i]<0)
                   }),
                   year5=round(sapply(locpredict, function(lfun) {
                       do.call(lfun, list(5, median))
                   }),1),
                   year10=round(sapply(locpredict, function(lfun) {
                       do.call(lfun, list(10, median))
                   }),1))

yrs <- 0:10
df <- expand.grid(1:14, yrs)
df$pop <- sapply(1:nrow(df), function(n) {
    i <- df[n,1]
    j <- df[n,2]
    round(locpredict[[i]](j, median),1)
})
df$location <- rep(locs, length(yrs))
names(df) <- c('ID','YearID','P_Otters','Site')
df$Year <- df$YearID + 2012
attributes(df)$out.attrs <- NULL
dfm <- merge(df, otr2, all.x = TRUE)


popplot <- ggplot(data=dfm) + geom_line(aes(x=Year, y=P_Otters, color=Site))
popplot <- popplot + geom_point(aes(x=Year, y=O_Otters, color=Site))
popplot <- popplot + facet_wrap(~Site) + theme_classic()


predict10 <- sapply(locpredict, function(lfun) {
    do.call(lfun, list(10, median))
})


