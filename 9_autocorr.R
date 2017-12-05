datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
library(spdep)
library(raster)


av <- read.csv(file.path(datapath, 'allvars.csv'))

####################################################################
####################################################################

avsp <- SpatialPointsDataFrame(coords=av[,c('Longitude','Latitude'), ],
                               proj4string=CRS("+init=epsg:4326"), data=av)


                               

