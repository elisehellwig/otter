datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
library(rstan)

pupr <- read.csv(file.path(datapath, 'pupclean.csv'))
linear <- readRDS(file.path(datapath, 'models/puplinear.RDS'))
pvl1 <- readRDS(file.path(datapath, 'models/puprvarying1location.RDS'))
pvl2 <- readRDS(file.path(datapath, 'models/puprvarying2location.RDS'))

source('functions.R')


##########################################################

locfac <- factor(pupr$location)

#data for lm stan model
fixedlist <- list(pups=pupr$pups,
                  year=pupr$year,
                  N=nrow(pupr))

multilist <- list(pups=pupr$pups,
                  year=pupr$year,
                  N=nrow(pupr),
                  P=nlevels(locfac),
                  loc=as.integer(locfac))



##################################
#otter population models


#fixed FX model
fixedl <- stan(model_code=linear, data=fixedlist, iter=20000, warmup = 5000,
               chains=1)
saveRDS(fixedl, file.path(datapath, 'models/pupfixedpost.RDS'))

#Varying FX location model

#identifying any pathologies in the model
multi1 <- stan(model_code=pvl1, data=multilist, iter=20000, warmup=5000,
               chains=1, control=list(adapt_delta = 0.99))

saveRDS(multi1, file.path(datapath, 'models/pupvarying1locationpost.RDS'))


#running the model for estimating parameters etc.
multi2 <- stan(model_code=pvl2, data=multilist, iter=20000, warmup=5000,
                  chains=4, control=list(adapt_delta = 0.99))
saveRDS(multi2, file.path(datapath, 'models/pupvarying2locationpost.RDS'))







