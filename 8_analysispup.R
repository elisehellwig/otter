datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(rstan)
library(ggplot2)
library(rethinking)
library(rstanarm)
pup <- read.csv(file.path(datapath, 'pupclean.csv'))
vl1 <- readRDS(file.path(datapath, 'models/pupvarying1locationpost.RDS'))
vl2 <- readRDS(file.path(datapath, 'models/pupvarying2locationpost.RDS'))


source('functions.R')

locs <- levels(factor(otr$location))
pup <- pup[,c('location','year','pups')]


####################################################################
####################################################################


puppoisfixed <- stan_glm(pups ~ year, data=pup, family=poisson)


