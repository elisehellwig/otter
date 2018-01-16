library(ggplot2)
library(rgdal)
library(dismo)
library(tmap)
library(tmaptools)

datapath <- '/Users/echellwig/Drive/OtherPeople/otterData/'
pp <- read.csv(file.path(datapath, 'vis/popplot.csv'))

lvlNames <- c('Abbotts Lagoon', 'Alpine Reservoir', 'Bass Lake','Bolinas',
              'Drakes Bay', 'El Estero', 'Giacomini Wetlands','Las Gallinas',
              'Madera Creek', 'Muir Woods','North Tomales Bay','Peters Dam',
              'Rodeo Lagoon','Tennessee Valley')
levels(pp$Site) <- lvlNames
####################################################

popplot <- ggplot(data=pp) + geom_line(aes(x=Year, y=P_Otters), size=1.2)
popplot <- popplot + geom_ribbon(aes(x=Year, min=Lower95, max=Upper95), 
                                 alpha=0.3)
popplot <- popplot + geom_point(aes(x=Year, y=O_Otters), size=3)
popplot <- popplot + facet_wrap(~Site) + theme_bw(30)
popplot <- popplot + scale_y_continuous(breaks=seq(0, 15, by=5))
popplot <- popplot + scale_x_continuous(breaks=seq(2013, 2021, by=2))
popplot <- popplot + coord_cartesian(ylim = c(0, 15)) 
popplot <- popplot + labs(x='Year', y='Otter Population')
#popplot <- popplot + geom_hline(yintercept=0, color='red2', size=1.3)

png(file.path(datapath, 'plots/population.png'), width=3500, height=2000, 
    res=150)
popplot
dev.off()

####################################################
# map of locations
locs <- readOGR(dsn=file.path(datapath, 'otterSPDF.GeoJSON'),layer="OGRGeoJSON")


data(locs)
tm_shape(locs) + tm_shape('')

data(land)
tm_shape(land)


qtm(shp=locs, text='loc', scale=1.5) + tm_layout(attr.just=c('left','top'))

locbase <- gmap(extent(locs), type='satellite', lonlat=TRUE, scale=2)
crs(locs) <- crs(locbase)


tcoords <- coordinates(locs)
tcoords[,1] <- tcoords[,1] + 0.005
tcoords[1,2] <- tcoords[1,2]-0.005


png(file.path(datapath, 'plots/locations.png'), width=3000, height=3000, 
    res=150)
    plot(locbase)
    plot(locs, pch=19, cex=3, col='maroon3', add=TRUE)
    text(tcoords[,1], tcoords[,2], labels=locs$loc, pos=4, col='white', cex=3)
dev.off()

####################################################
####################################################
fullrf <- readRDS(file.path(datapath, 'fullrandomForest.RDS'))
selrf <- readRDS(file.path(datapath, 'selectrandomForest.RDS'))

vars <- c('Initial Population Size', 'Change in Population size',
          'Probability of Decline')


png(file.path(datapath, 'plots/fullrf.png'), width=3000, height=1000, 
    res=150)
    par(mfrow=c(1,3), cex=1.7, mar=c(3,2,2,1))
    for (i in 1:length(fullrf)){
        varImpPlot(fullrf[[i]], main=vars[i])
    }
dev.off()


png(file.path(datapath, 'plots/selectrf.png'), width=3000, height=1000, 
    res=150)
par(mfrow=c(1,3), cex=1.7, mar=c(3,2,2,1))
for (i in 1:length(selrf)){
    varImpPlot(selrf[[i]], main=vars[i])
}
dev.off()

####################################################
####################################################
osp <- readOGR(dsn=file.path(datapath, 'otterSPDF.GeoJSON'),
               layer="OGRGeoJSON")
bgosp <- gmap(extent(osp), type='satellite', lonlat=TRUE, scale=2)
knb <- readRDS(file.path(datapath, 'kNearOtters.RDS'))
distnb <- readRDS(file.path(datapath, 'DistanceOtters.RDS'))
xy <- coordinates(osp)

png(file.path(datapath, 'plots/adjacenciesK.png'), width=3000, height=2000, 
    res=150)
    plot(bgosp)
    plot(knb, xy, col='lightblue', lwd=4, add=TRUE)
    plot(osp, pch=19, col='red', add=TRUE, cex=2.5)
dev.off()

png(file.path(datapath, 'plots/adjacenciesD.png'), width=3000, height=2000, 
    res=150)
    plot(bgosp)
    plot(distnb, xy, col='lightblue', lwd=4, add=TRUE)
    plot(osp, pch=19, cex=2.5, col='red', add=TRUE)
dev.off()

####################################################
####################################################

avm <- read.csv(file.path(datapath, 'LatitudeModelResults.csv'))

av_alpha <- avm[avm$response=='alpha', ]
areg <- ggplot() + geom_point(aes(x=Latitude, y=value),
                              data=av_alpha[av_alpha$type=='obs',], size=2.7)
areg <- areg + geom_line(aes(x=Latitude, y=value), color='navy',
                         size=1.5, data=av_alpha[av_alpha$type=='fit',])
#areg <- areg + geom_text(aes(x=38, y=3, label=formulastring(Amod)), hjust=0,
                        # size=6.5, color='navy')
areg <- areg + labs(y='Mean Initial Population Size (Otters)') + theme_bw(17)

png(file.path(datapath, 'plots/LatAlpha.png'), width=1000, height = 800, 
    res=150)
    areg
dev.off()


av_beta <- avm[avm$response=='beta', ]
breg <- ggplot() + geom_point(aes(x=Latitude, y=value), size=2.7,
                              data=av_beta[av_beta$type=='obs',])
breg <- breg + geom_line(aes(x=Latitude, y=value), color='red4',
                         size=1.5, data=av_beta[av_beta$type=='fit',])
#breg <- breg + geom_text(aes(x=38, y=-0.25, label=formulastring(Bmod)),
                         #hjust=0, size=5.5, color='red4')
breg <- breg + labs(y='Annual Change in Otter Population')+ theme_bw(17)

png(file.path(datapath, 'plots/LatBeta.png'), width=1000, height = 800, 
    res=150)
breg
dev.off()


