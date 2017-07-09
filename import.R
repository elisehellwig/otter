datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
library(reshape2)

options(stringsAsFactors = FALSE)

ott <- read.csv(file.path(datapath, 'otterpopAll.csv'))
pup <- read.csv(file.path(datapath, 'otterpopPups.csv'))


areaNames <- c('Abbots', "NTB", "Giacomini", 'Rodeo', "Muir",'Madera',
               'Tennessee', "Bolinas",'Drakes','Bass','Alpine','Peters',
               'LasGallinas','Estero')
sector <- c('North','North','North','South','South','Bay','South','South',
            'North', 'Coast', 'Lagunitas','Lagunitas', 'Bay','North') 

#####################################################

ott$Area <- areaNames
ott$sector <- sector
ottm <- melt(ott, id.vars = c('Area','sector'), variable.name = 'Year', value.name = 'Pop')

ottm$Year <- as.integer(sub("X", "", ottm$Year))

NApops <- which(is.na(ottm$Pop))
ottr <- ottm[-NApops,]

write.csv(ottr, file.path(datapath, 'otterclean.csv'), row.names = FALSE)



pup$Area <- areaNames
pup$sector <- sector
pupm <- melt(pup, id.vars = c('Area','sector'), variable.name = 'Year', value.name = 'Pop')

pupm$Year <- as.integer(sub("X", "", pupm$Year))

NApops <- which(is.na(pupm$Pop))
pupr <- pupm[-NApops,]

write.csv(pupr, file.path(datapath, 'pupclean.csv'), row.names = FALSE)



