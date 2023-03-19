path <- '/Users/echellwig/Google Drive/OtherPeople/otterData'

library(data.table)
library(rethinking)
library(ggplot2)
library(sf)

hex <- fread(file.path(path, 'results/OtterSpotter/HexEstimates.csv'))
uhex <- fread(file.path(path, 'results/OtterSpotter/HexUncertainty.csv'))
modv <- readRDS(file.path(path, 'models/OtterSpotter/InterceptVarying.RDS'))

shp_fn <- "raw/OS Hexagon 154SQKM Shapefile/GenerateTessellation154SQKM.shp"
hexshp0 <- st_read(file.path(path, shp_fn))

ids0 <- fread(file.path(path, 'clean/OtterSpotter2020.csv'))
ids <- unique(ids0[,.(Hex, ShapeID)])

ta <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# Some analysis -----------------------------------------------------------

vsamp <- extract.samples(modv, n=1e5)
beta_bar_est <- exp(median(vsamp$beta_bar))

hex[,Quant:=cut(Varying, breaks=quantile(Varying), include.lowest=TRUE, labels=FALSE)]

hexm <- melt(hex, id.vars = c('HexID', 'Hex', 'PlotOrder', 'Quant', 'ShapeID'), 
             variable.name = 'Type', value.name = 'PopEst')


# Normal Plots ------------------------------------------------------------


pplot <- ggplot(data=hexm) +
    geom_point(aes(x=PlotOrder, y=PopEst, shape=Type)) +
    geom_hline(yintercept=beta_bar_est, color='grey', linetype='dashed') +
    scale_shape_manual(values=c(4,1,16), 
                       labels=c('Raw Avg', 'Fixed FX Model','Varying FX Model')) +
    labs(x='Hex', y='Otter Population Estimate', shape='Estimate Type') +
    theme(axis.text.x=element_blank(), axis.ticks.x = element_blank()) +
    theme_bw()

uplot <- ggplot(data=uhex) +
    geom_pointrange(aes(x=PlotOrder, y=PopEst, ymin=Lower95, ymax=Upper95))+
    labs(title='Otter Population Estimate with 95% Confidence Intervals',
         x='Hex', y='Otter Population Estimate') +
    theme(axis.text.x=element_blank(), axis.ticks.x = element_blank()) +
    theme_bw()



# Joins -------------------------------------------------------------------

names(hexshp0)[3] <- 'ShapeID'

hexid <- merge(ids, hex, by=c('Hex', 'ShapeID'), all.x=TRUE)

hexshp <- merge(hexshp0, hexid, by='ShapeID')
hexshp <- hexshp[,c('ShapeID', 'Hex', 'HexID', 'Varying', 'Quant')]

hexta <- st_transform(hexshp, crs=ta)
hexta[is.na(hexta$Quant), 'Quant'] <- 0



plot(hexta["Quant"], col=c('#969696','#ffffb2','#fecc5c','#fd8d3c','#e31a1c'))

# Spatial Plots -----------------------------------------------------------

colpal <- data.frame(Quant=0:4, Color=c('#969696','#ffffb2','#fecc5c','#fd8d3c','#e31a1c'))
hexta <- merge(hexta, colpal, by='Quant')

plot(hexta["Quant"], col=hexta$Color, main='Otter Population Quartiles')
