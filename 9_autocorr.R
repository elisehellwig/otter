datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
library(spdep)
library(raster)


av <- read.csv(file.path(datapath, 'allvars.csv'))

####################################################################
####################################################################

#9km

avsp <- SpatialPointsDataFrame(coords=av[,c('Longitude','Latitude'), ],
                               proj4string=CRS("+init=epsg:4326"), data=av)

knear <- knearneigh(avsp, k=3, longlat=TRUE)
knb <- knn2nb(knear, row.names = avsp$loc)

distnb <- dnearneigh(avsp, d1=0, d2=10, longlat=TRUE, row.names=avsp$loc)


klist <- nb2listw(knb)
distlist <- nb2listw(distnb)
kmoran <- moran.mc(avsp$metric, klist, 9999)
distmoran <- moran.mc(avsp$metric, distlist, 9999)


                               

