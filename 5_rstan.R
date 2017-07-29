datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
library(rstan)

otr <- read.csv(file.path(datapath, 'otterclean.csv'))
linear <- readRDS(file.path(datapath, 'models/linear.RDS'))
linearsplit <- readRDS(file.path(datapath, 'models/linearsplit.RDS'))
vl <- readRDS(file.path(datapath, 'models/varyinglocation.RDS'))

source('functions.R')

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

multilist <- list(pop=otr$pop,
                  year=otr$year,
                  N=nrow(otr),
                  P=nlevels(locfac),
                  loc=as.integer(locfac))



##################################
#otter population models


#fixed FX model
fixedl <- stan(model_code=linear, data=fixedlist, iter=20000, warmup = 5000,
               chains=1)
saveRDS(multilong, file.path(datapath, 'models/fixedpost.RDS'))


#fixed split FX model
fixedsplit <- stan(model_code=linearsplit, data=splitlist, iter=4000, 
                   warmup = 2000, chains=4)



#Varying FX location model

#identifying any pathologies in the model
#multi <- stan(model_code=vl, data=multilist, iter=8000, chains=4, 
             # control=list(adapt_delta = 0.99))

#running the model for estimating parameters etc.
multilong <- stan(model_code=vl, data=multilist, iter=20000, warmup=5000,
                  chains=1, control=list(adapt_delta = 0.99))
saveRDS(multilong, file.path(datapath, 'models/varyinglocationpost.RDS'))

multipost <- locpost(multilong, 6)

multidecline <- sapply(1:13, function(i) declinerisk(multilong, i, locpost)) 
multibeta2 <- sapply(1:13, function(i) mean(locpost(multilong, i)$b1))
multidf <- data.frame(location=levels(locfac),
                      b1=round(multibeta2, 3),
                      declineprob=round(multidecline, 3))



library(rethinking)
coeftab(multilong)
