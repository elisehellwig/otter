datapath <- '/Users/echellwig/Google Drive/OtherPeople/otterData'


library(rstan)
library(rethinking)
library(data.table)


spot <- fread(file.path(datapath, 'clean/OtterSpotter2020Filtered.csv'))
linearfixed <- readRDS(file.path(datapath, 'fixedlinear.RDS'))

raw_mu <- spot[,.(RawPop=mean(MaxOtters)), by=.(HexID, Hex, ShapeID)]


dat <- list(L=spot$MaxOtters,
            hex=spot$HexID,
            X=rep(1, nrow(spot)))


modf <- ulam(
    alist(
        L ~ dpois(lambda),
        log(lambda) <- beta[hex],
        beta[hex] ~ dnorm(0, 1.5)
    ), data=dat, chains=1, iter=1e5, log_lik = TRUE
)

logsampF <- extract.samples(modf, n=nrow(raw_mu)*1e5)
Fest <- exp(apply(logsampF$beta, 2, median))

modv <- ulam(
    alist(
        L ~ dpois(lambda),
        log(lambda) <-  beta[hex],
        beta[hex] ~ dnorm(beta_bar, sigma),
        beta_bar ~ dnorm(0, 1.5),
        sigma ~ dexp(1)
    ), data=dat, chains=1, iter=1e5, log_lik = TRUE
)

compare(modf, modv)

logsampV <- extract.samples(modv, n=nrow(raw_mu)*1e5)
sampVpi <- t(apply(logsampV$beta, 2, HPDI, prob=95))

Vest <- exp(apply(logsampV$beta, 2, median))

vcertain <- exp(t(apply(logsampV$beta, 2, HPDI, prob=95)))
 

compdf0 <- data.table(Fixed=Fest, Varying=Vest)
compdf <- cbind(raw_mu, compdf0)
compdf[,Quant:=cut(Varying, breaks=quantile(Varying), include.lowest=TRUE, labels=FALSE)]

setkey(compdf, Varying)

compdf[, PlotOrder:=1:nrow(compdf)]

uncertain0 <- cbind(raw_mu, Vest, vcertain) 
unames <- c('HexID', 'Hex', 'ShapeID', 'RawAvg', 'PopEst', 'Lower95', 'Upper95')
setnames(uncertain0, names(uncertain0), unames)

uncertain <- merge(uncertain0, compdf[,.(HexID, PlotOrder)], by='HexID')

fwrite(compdf, file.path(datapath, 'results/OtterSpotter/HexEstimates.csv'))
fwrite(uncertain, file.path(datapath, 'results/OtterSpotter/HexUncertainty.csv'))
saveRDS(modf, file.path(datapath, 'models/OtterSpotter/InterceptFixed.RDS'))
saveRDS(modv, file.path(datapath, 'models/OtterSpotter/InterceptVarying.RDS'))
