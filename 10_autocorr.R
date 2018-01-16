datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
library(spdep)
library(raster)
library(rgdal)
source('functions.R')

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

p <- 99999

vars <- c('alpha','beta','declineP')
spID <- c(1,2) #1=KNN, 2=Distance based (10km)
h1 <- c('greater','less')
resid <- c(TRUE, FALSE)

ao <- expand.grid(resid, h1, spID, vars, stringsAsFactors = FALSE)
names(ao) <- c('resid', 'h1', 'spID','var')
splist <- list(klist, distlist)


set.seed(4678)
pvals <- sapply(1:nrow(ao), function(i) {
    autocor(rsp, ao[i,'var'], splist[[ ao[i,'spID'] ]], p, 
            res=ao[i, 'resid'], alt=ao[i,'h1'], return='p-value')
})

set.seed(4678)
morans <- sapply(1:nrow(ao), function(i) {
    autocor(rsp, ao[i,'var'], splist[[ ao[i,'spID'] ]], p, 
            res=ao[i, 'resid'], alt=ao[i,'h1'], return='statistic')
})

morandf <- data.frame(var=ao$var,
                 spmethod=ifelse(ao$spID==1, 'knn', 'dist'),
                 h1=ao$h1,
                 res=ifelse(ao$resid, 'residuals', 'originalData'),
                 Moran=morans,
                 pval=pvals)

write.csv(morandf, file.path(datapath, 'moranI.csv'), row.names = FALSE)
=======
set.seed(4678)
moranG <- data.frame(knn=sapply(vars, function(v) {
                      autocor(rsp, v, klist, p, return='statistic')
                       }),  
                    knnRES=sapply(vars, function(v) {
                       autocor(rsp, v, klist, p, res=TRUE, return='statistic')
                        }),
                    dist=sapply(vars, function(v) {
                       autocor(rsp, v, distlist, p, return='statistic')
                        }),
                    distRES=sapply(vars, function(v) {
                       autocor(rsp, v, distlist, p, res=TRUE, 
                               return='statistic')
                        }))





pval <- data.frame(knn=sapply(moranG$knn, function(mi) mi$p.value),
                   knnRES=sapply(moranG$knnRES, function(mi) mi$p.value),
                   dist=sapply(moranG$dist, function(mi) mi$p.value),
                   distRES=sapply(moranG$distRES, function(mi) mi$p.value))

>>>>>>> aea983209de908bb3e9277e67e3da12ad58454d0
