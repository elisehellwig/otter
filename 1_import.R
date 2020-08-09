datapath <- '/Users/echellwig/Google Drive/OtherPeople/otterData/'
library(data.table)

options(stringsAsFactors = FALSE)

locs <- fread(file.path(datapath, 'otterlocs.csv'))
ott <- fread(file.path(datapath, 'OtterPopulation2019.csv'), header=TRUE)

areaNames <- c('Abbotts', "NTB", "Giacomini", 'Rodeo', "Muir",'Greenbrae',
               'Tennessee', "Bolinas",'Drakes','Bass', 'Reservoir','Lagunitas',
               'LasGallinas','Estero')

#####################################################

ott[,":="(location=areaNames, FSS=NULL)]

ott_all <- merge(ott, locs, by='location')

ottm <- melt(ott_all, id.vars = c('location','region','habitat'), 
             variable.name = 'year', value.name = 'pop', variable.factor = FALSE)

ottm[,":="(year=as.numeric(year))]

ottmNApops <- which(is.na(ottm$pop))
ottr <- ottm[!is.na(pop)]

ottr$year <- ottr$year-2012

ottr <- ottr[!location %in% c('Greenbrae', 'Bolinas')]

write.csv(ottr, file.path(datapath, 'otterclean2019.csv'), row.names = FALSE)



pup$Area <- areaNames
names(pup)[1] <- 'location'
pup_all <- merge(pup, locs, by='location')


pupm <- melt(pup_all, id.vars = c('location','region','habitat'), 
             variable.name = 'year', value.name = 'pups')

pupm$year <- as.integer(sub("X", "", pupm$year))

NApops <- which(is.na(pupm$pups))
pupr <- pupm[-NApops,]

pupr$year <- pupr$year - 2012

otterpupper <- merge(ottr, pupr)

write.csv(otterpupper, file.path(datapath, 'pupclean.csv'), row.names = FALSE)



