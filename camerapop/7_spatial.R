datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
library(raster)
library(rgeos)
library(rgdal)
library(ggplot2)
otr <- read.csv(file.path(datapath, 'otterclean.csv'))
rv <- read.csv(file.path(datapath,'responsevars.csv'))
sl <- read.csv(file.path(datapath, 'sitelocations.csv'))
visit <- read.csv(file.path(datapath, 'annualvisits.csv'))

pdens <- readOGR(dsn=file.path(datapath, 'SubCountyPopDensity'),
              layer='CA_subcountyPopDen')
names(pdens) <- c('Name','PopDensity','Census')
####################################################################
pdens$logDensity <- log(pdens$PopDensity)

names(sl)[1] <- 'loc'
names(otr)[1] <- 'loc'
names(visit) <- c('loc', 'visit')
otrlocs <- unique(otr[, c('loc','region','habitat')])

rv$metric <- rv$alpha * rv$beta
rv$ID <- 1:14

x <- merge(rv, sl)
x <- merge(x, otrlocs)
x <- merge(x, visit)
####################################################################
####################################################################
xsp <- x
coordinates(xsp) <-~ Longitude + Latitude
crs(xsp) <- crs(pdens)

pdenspoints <- over(xsp,pdens)[,c(2,4)]
pdenspoints$ID <- 1:14

xsp <- merge(xsp, pdenspoints)

coorddf <- as.data.frame(coordinates(xsp))
coorddf$ID <- 1:14
xsp <- merge(xsp, coorddf)

xspdf <- xsp@data

xspdf$alpha <- round(xspdf$alpha, 2)
xspdf$beta <- round(xspdf$beta, 3)
xspdf$metric <- round(xspdf$metric, 2)

xspdf$latscale <- scale(xspdf$Latitude)
attributes(xspdf$latscale) <- NULL

write.csv(xspdf, file.path(datapath, 'allvars.csv'), row.names = FALSE)



