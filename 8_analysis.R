datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(rstan)

otr <- read.csv(file.path(datapath, 'otterlocs.csv'))
vl1 <- readRDS(file.path(datapath, 'models/varying1locationpost.RDS'))
vl2 <- readRDS(file.path(datapath, 'models/varying2locationpost.RDS'))


source('functions.R')

locs <- levels(factor(otr$location))
