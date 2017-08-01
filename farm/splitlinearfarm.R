options(stringsAsFactors = FALSE)
options(mc.cores = 8)
library(parallel)
library(rstan)

otr <- read.csv('otterclean.csv')
linearsplit <- readRDS('linearsplit.RDS')


otr$cpop <- scale(otr$pop) #4.62 center; 2.15 scale
attributes(otr$cpop) <- NULL
##########################################################

locfac <- factor(otr$location)

binaryvars <- model.matrix(~location, data=otr)[,-1]

bvlist <- lapply(1:12, function(i) {
    unname(binaryvars[,i])
})
names(bvlist) <- levels(locfac)[-1]


#data for lm stan model
fixedlist <- list(pop=otr$pop,
                  year=otr$year,
                  N=nrow(otr))

splitlist <- c(fixedlist, bvlist)



##################################
#otter population models


#fixed split FX model
fixedsplit <- stan(model_code=linearsplit, data=splitlist, iter=4000, 
                   warmup = 2000, chains=4, control=list(max_treedepth=15))


saveRDS(fixedsplit, 'fixedsplitpost.RDS')
