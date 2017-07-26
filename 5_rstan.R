datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
library(rstan)

otr <- read.csv(file.path(datapath, 'otterclean.csv'))
