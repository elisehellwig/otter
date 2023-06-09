datapath <- '/Users/echellwig/Google Drive/OtherPeople/otterData/'
options(stringsAsFactors = FALSE)
library(rstan)
library(ggplot2)
library(rethinking)
otr <- read.csv(file.path(datapath, 'otterclean2019.csv'))
vl1 <- readRDS(file.path(datapath, 'models/varying1locationpost.RDS'))
vl2 <- readRDS(file.path(datapath, 'models/varying2locationpost.RDS'))
vlP <- readRDS(file.path(datapath, 'models/varyinglocationPOIS.RDS'))


source('functions.R')

locs <- levels(factor(otr$location))

###########################################################################
otr2 <- otr[,c('location','year','pop')]
names(otr2) <- c('Site','YearID','O_Otters')

###########################################################################

##param analysis

b0fixed <- extractpar(vl2, 'beta', location=NA, rows=1)
b1fixed <- extractpar(vl2, 'beta', location=NA, rows=2)

b0samples <- data.frame(sapply(1:length(locs), function(loc) {
    extractpar(vl2, 'u', location=loc, rows=1) + b0fixed
}))
names(b0samples) <- locs


####################
##preping for 
b1samples <- data.frame(sapply(1:length(locs), function(loc) {
    extractpar(vl2, 'u', location=loc, rows=2) + b1fixed
}))
names(b1samples) <- locs


b0quants <- t(round(apply(b0samples, 2, quantile, 
                         probs=c(0.025, .10, .25, .5, .75,.9, .975))))
b1quants <- t(round(apply(b1samples, 2, quantile,prob=c(0.5 ,0.025, 0.975)),3))
b1quants <- data.frame(b1quants)
b1quants$locs <- rownames(b1quants)
names(b1quants) <- c('estimate','lower95','upper95','loc')
write.csv(b1quants, file.path(datapath, 'beta1quantiles.csv'),
          row.names = FALSE)

b1quants$locs <- rownames(b1quants)

locpredict <- sapply(1:length(locs), function(i) {
    locpost(vl2, i, 'u', response='function')
})



yrs <- 0:15
df <- expand.grid(1:length(locs), yrs)
attributes(df)$out.attrs <- NULL
df$pop <- predictpop(df, locpredict, median)
allpredictions <- predictpop(df, locpredict, same)
hpdints <- t(apply(allpredictions, 2, HPDI, prob=0.95))
attributes(hpdints)$dimnames <- NULL

dfci <- cbind(df, hpdints)

dfci$location <- rep(locs, length(yrs))
names(dfci) <- c('ID','YearID','P_Otters','Lower95','Upper95','Site')
dfci$Year <- dfci$YearID + 2012
dfm <- merge(dfci, otr2, all.x = TRUE)

dpSites <- c()
posSites <- c('NTB')
dfm$SiteColor <- ifelse(dfm$Site %in% posSites, "Pos", "None")
dfm[dfm$Site %in% dpSites, 'SiteColor'] <- 'Neg'

write.csv(dfm, file.path(datapath, 'vis/popplot.csv'), row.names = FALSE)


predict10 <- sapply(locpredict, function(lfun) {
    do.call(lfun, list(10, median))
})

extinctp10 <- sapply(locpredict, function(lfun) {
    mean(do.call(lfun, list(10, same))<0)
})


pdat <- data.frame(loc=locs,
                   declineP=sapply(1:length(locs), function(i) {
                       mean(b1samples[,i]<0)
                   }),
                   extinct10=extinctp10,
                   year5=round(sapply(locpredict, function(lfun) {
                       do.call(lfun, list(5, median))
                   }),1),
                   year10=round(sapply(locpredict, function(lfun) {
                       do.call(lfun, list(10, median))
                   }),1))

write.csv(pdat, file.path(datapath,'vis/likelihoods.csv'), row.names = FALSE)


paramdf <- data.frame(alpha=apply(b0samples, 2, mean),
                      beta=apply(b1samples, 2, mean))
responsedf <- cbind(pdat, paramdf)

write.csv(responsedf, file.path(datapath,'responsevars.csv'), row.names = FALSE)


