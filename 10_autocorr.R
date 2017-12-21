datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
library(spdep)
library(raster)
library(rgdal)


rdf <- read.csv(file.path(datapath, 'Residuals.csv'))


####################################################################
####################################################################

#9km
rsp <- SpatialPointsDataFrame(coords=rdf[,c('Longitude','Latitude'), ],
                               proj4string=CRS("+init=epsg:4326"), data=rdf)

writeOGR(rsp, dsn=file.path(datapath, 'otterSPDF.GeoJSON'),
         layer='"OGRGeoJSON"', driver='GeoJSON')

knear <- knearneigh(rsp, k=3, longlat=TRUE)
knb <- knn2nb(knear, row.names = rsp$loc)
saveRDS(knb, file.path(datapath, 'kNearOtters.RDS'))

distnb <- dnearneigh(rsp, d1=0, d2=10, longlat=TRUE, row.names=rsp$loc)
saveRDS(distnb, file.path(datapath, 'DistanceOtters.RDS'))


klist <- nb2listw(knb)
distlist <- nb2listw(distnb)

set.seed(2818893)
### Alpha ######
kalpha <- moran.mc(rsp$alpha, klist, 99999)
distalpha <- moran.mc(rsp$alpha, distlist, 99999)

kresAG <- moran.mc(rsp$alphares, klist, 99999, alternative='greater')
kresAL <- moran.mc(rsp$alphares, klist, 99999, alternative='less')
distresAG <- moran.mc(rsp$alphares, distlist, 99999, 
                      alternative='greater')
distresAL <- moran.mc(rsp$alphares, distlist, 99999, alternative='less')


### Beta ######

kbeta <- moran.mc(rsp$beta, klist, 99999)
distbeta <- moran.mc(rsp$beta, distlist, 99999)

kresBG <- moran.mc(rsp$betares, klist, 99999, alternative='greater')
kresBL <- moran.mc(rsp$betares, klist, 99999, alternative='less')
distresBG <- moran.mc(rsp$betares, distlist, 99999, 
                      alternative='greater')
distresBL <- moran.mc(rsp$betares, distlist, 99999, alternative='less')

### DeclineP ######

kDP <- moran.mc(rsp$declineP, klist, 99999)
distDP <- moran.mc(rsp$declineP, distlist, 99999)

kresDP <- moran.mc(rsp$declinePres, klist, 99999)
distresDP <- moran.mc(rsp$declinePres, distlist, 99999)
