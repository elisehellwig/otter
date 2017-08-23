datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(rstan)

otr <- read.csv(file.path(datapath, 'otterlocs.csv'))
vl1 <- readRDS(file.path(datapath, 'models/varying1locationpost.RDS'))
vl2 <- readRDS(file.path(datapath, 'models/varying2locationpost.RDS'))

source('functions.R')

locs <- levels(factor(otr$location))


###########################################################

pars2 <- extract(vl2)

u2 <- as.data.frame(extract(vl2, pars='u'))
betas2 <- grep("u.2.", names(u2))
u22 <- u2[,betas2]

u22 <- extractStanPars(vl2, 'u', 2, stat=mean, columnnames=locs)
otrr <- otr[-1, ]

lvl2 <- merge(otrr, u22)

mod1 <- lm(estimate ~ region, data=lvl2)










