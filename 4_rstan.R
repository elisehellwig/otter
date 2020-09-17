
# Update This -------------------------------------------------------------

year <- 2019


# Setup -------------------------------------------------------------------


datapath <- '/Users/echellwig/Google Drive/OtherPeople/otterData/'
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
library(rstan)

rstan_options(auto_write = TRUE)


# Read In -----------------------------------------------------------------


otr <- read.csv(file.path(datapath, 'clean', paste0('otterclean', year, 'csv')))
linear <- readRDS(file.path(datapath, 'models/cpp/linear.RDS'))
vl1 <- readRDS(file.path(datapath, 'models/cpp/varying1location.RDS'))
vl2 <- readRDS(file.path(datapath, 'models/cpp/varying2location.RDS'))
vlP <- readRDS(file.path(datapath, 'models/cpp/varyinglocationPOIS.RDS'))



source('functions.R')

#otr$cpop <- scale(otr$pop) #4.62 center; 2.15 scale
#attributes(otr$cpop) <- NULL

#note if a prior is not specified it is uniform.
##########################################################

#data for lm stan model
fixedlist <- list(pop=otr$pop,
                  year=otr$year,
                  N=nrow(otr))

multilist <- list(pop=otr$pop,
                  year=otr$year,
                  N=nrow(otr),
                  P=length(unique(otr$siteID)),
                  loc=otr$siteID)



##################################
#otter population models


#fixed FX model
fixedl <- stan(model_code=linear, data=fixedlist, iter=20000, warmup = 5000,
               chains=1)
saveRDS(fixedl, file.path(datapath, 'models/fixedpost.RDS'))



# Identifying Pathologies in the models -----------------------------------

#varying effects on intercepts
multi1 <- stan(model_code=vl1, data=multilist, iter=25000, warmup=5000,
               chains=1, control=list(adapt_delta = 0.99))

saveRDS(multi1, file.path(datapath, 'models/varying1locationpost.RDS'))

#varying effects on intercepts + slopes
multi2 <- stan(model_code=vl2, data=multilist, iter=10000, warmup=1000,
               chains=4, control=list(adapt_delta = 0.99))

#varying effects glm with poisson distribution
multiP <- stan(model_code=vlP, data=multilist, iter=10000, warmup=1000,
               chains=4, control=list(adapt_delta = 0.99))


# Run Model for parameter estimation --------------------------------------

#mixed effects model
multi2long <- stan(model_code=vl2, data=multilist, iter=25000, warmup=5000,
                  chains=1, control=list(adapt_delta = 0.99))
saveRDS(multi2long, file.path(datapath, 'models/varying2locationpost.RDS'))


#mixed effects glm
multiPlong <- stan(model_code=vlP, data=multilist, iter=25000, 
                   warmup=5000, chains=1, control=list(adapt_delta = 0.99))
saveRDS(multiPlong, file.path(datapath, 'models/varyinglocPOISpost.RDS'))



