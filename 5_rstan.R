datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
library(rstan)

otr <- read.csv(file.path(datapath, 'otterclean.csv'))
linear <- readRDS(file.path(datapath, 'models/linear.RDS'))
vl1 <- readRDS(file.path(datapath, 'models/varying1location.RDS'))
vl2 <- readRDS(file.path(datapath, 'models/varying2location.RDS'))

source('functions.R')

otr$cpop <- scale(otr$pop) #4.62 center; 2.15 scale
attributes(otr$cpop) <- NULL
##########################################################

locfac <- factor(otr$location)

#data for lm stan model
fixedlist <- list(pop=otr$pop,
                  year=otr$year,
                  N=nrow(otr))

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
saveRDS(fixedl, file.path(datapath, 'models/fixedpost.RDS'))

#Varying FX location model

#identifying any pathologies in the model
multi1 <- stan(model_code=vl1, data=multilist, iter=25000, warmup=5000,
               chains=1, control=list(adapt_delta = 0.99))

saveRDS(multi1, file.path(datapath, 'models/varying1locationpost.RDS'))


#running the model for estimating parameters etc.
multi2long <- stan(model_code=vl, data=multilist, iter=20000, warmup=5000,
                  chains=1, control=list(adapt_delta = 0.99))
saveRDS(multilong, file.path(datapath, 'models/varying2locationpost.RDS'))


###########################################

multi1pup <- 

