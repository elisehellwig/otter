
# Update This -------------------------------------------------------------

year <- 2019


# Read in Data ------------------------------------------------------------

datapath <- '/Users/echellwig/Google Drive/OtherPeople/otterData/'
library(data.table)

locs <- fread(file.path(datapath, 'clean/otterlocs.csv'))

otterfile <- file.path(datapath, 'raw', paste0('OtterPopulation', year, '.csv'))
ott <- fread(otterfile, header=TRUE)



# Add Location info -------------------------------------------------------

ott[,FSS:=NULL]

ott_all <- merge(ott, locs[,.(siteID, location, region, habitat)], by='siteID')


# Reshape -----------------------------------------------------------------


ottm <- melt(ott_all, id.vars = c('siteID','location','region','habitat'), 
             variable.name = 'year', value.name = 'pop', 
             variable.factor = FALSE)

ottm[,year:=as.numeric(year)-2012]


# Remove Missing Data -----------------------------------------------------


ottr <- ottm[!is.na(pop)]

ottr <- ottr[!siteID %in% c(6,8)]


# Save --------------------------------------------------------------------

fwrite(ottr, file.path(datapath, 'clean/otterclean2019.csv'))


# 
# pup$Area <- areaNames
# names(pup)[1] <- 'location'
# pup_all <- merge(pup, locs, by='location')
# 
# 
# pupm <- melt(pup_all, id.vars = c('location','region','habitat'), 
#              variable.name = 'year', value.name = 'pups')
# 
# pupm$year <- as.integer(sub("X", "", pupm$year))
# 
# NApops <- which(is.na(pupm$pups))
# pupr <- pupm[-NApops,]
# 
# pupr$year <- pupr$year - 2012
# 
# otterpupper <- merge(ottr, pupr)
# 
# write.csv(otterpupper, file.path(datapath, 'pupclean.csv'), row.names = FALSE)
# 
# 
# 
