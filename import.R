datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
library(reshape2)

options(stringsAsFactors = FALSE)

locs <- read.csv(file.path(datapath, 'otterlocs.csv'))
ott <- read.csv(file.path(datapath, 'otterpopAll.csv'))
pup <- read.csv(file.path(datapath, 'otterpopPups.csv'))


areaNames <- c('Abbots', "NTB", "Giacomini", 'Rodeo', "Muir",'Madera',
               'Tennessee', "Bolinas",'Drakes','Bass','Alpine','Peters',
               'LasGallinas','Estero')

#####################################################

ott$Area <- areaNames
names(ott)[1] <- 'location'

ott_all <- merge(ott, locs, by='location')

ottm <- melt(ott_all, id.vars = c('location','region','habitat'), 
             variable.name = 'year', value.name = 'pop')

ottm$year <- as.integer(sub("X", "", ottm$year))

NApops <- which(is.na(ottm$pop))
ottr <- ottm[-NApops,]

write.csv(ottr, file.path(datapath, 'otterclean.csv'), row.names = FALSE)



pup$Area <- areaNames
names(pup)[1] <- 'location'
pup_all <- merge(pup, locs, by='location')


pupm <- melt(pup_all, id.vars = c('location','region','habitat'), 
             variable.name = 'year', value.name = 'pups')

pupm$year <- as.integer(sub("X", "", pupm$year))

NApops <- which(is.na(pupm$pups))
pupr <- pupm[-NApops,]

otterpupper <- merge(ottr, pupr)

write.csv(otterpupper, file.path(datapath, 'pupclean.csv'), row.names = FALSE)



