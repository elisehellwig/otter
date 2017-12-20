datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
library(spdep)
library(raster)
library(rgdal)


av <- read.csv(file.path(datapath, 'allvars.csv'))

####################################################################
####################################################################

#9km

avsp <- SpatialPointsDataFrame(coords=av[,c('Longitude','Latitude'), ],
                               proj4string=CRS("+init=epsg:4326"), data=av)

writeOGR(avsp, dsn=file.path(datapath, 'otterSPDF.GeoJSON'),
         layer='"OGRGeoJSON"', driver='GeoJSON')

knear <- knearneigh(avsp, k=3, longlat=TRUE)
knb <- knn2nb(knear, row.names = avsp$loc)
saveRDS(knb, file.path(datapath, 'kNearOtters.RDS'))

distnb <- dnearneigh(avsp, d1=0, d2=10, longlat=TRUE, row.names=avsp$loc)
saveRDS(distnb, file.path(datapath, 'DistanceOtters.RDS'))


klist <- nb2listw(knb)
distlist <- nb2listw(distnb)

kmetric <- moran.mc(avsp$metric, klist, 99999)
distmetric <- moran.mc(avsp$metric, distlist, 99999)

kalpha <- moran.mc(avsp$alpha, klist, 99999)
distalpha <- moran.mc(avsp$alpha, distlist, 99999)

kbeta <- moran.mc(avsp$beta, klist, 99999)
distbeta <- moran.mc(avsp$beta, distlist, 99999)

kDP <- moran.mc(avsp$declineP, klist, 99999)
distDP <- moran.mc(avsp$declineP, distlist, 99999)

kAres <- moran.mc(avsp$Ares, klist, 99999)
distAres <- moran.mc(avsp$Ares, distlist, 99999)
