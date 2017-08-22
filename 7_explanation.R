datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(rstan)


vl1 <- readRDS(file.path(datapath, 'models/varying1location.RDS'))
vl2 <- readRDS(file.path(datapath, 'models/varying2location.RDS'))

source('functions.R')
