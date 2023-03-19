datapath <- '/Users/echellwig/Google Drive/OtherPeople/otterData'

library(openxlsx)
library(data.table)
library(ggplot2)

idfn <- 'Otter Spotter Abundance Analysis Hexagon Dataset Working Copy Feb 7 2021.xlsx'

ids <- read.xlsx(file.path(datapath, 'raw', idfn), startRow=2, cols=1:2)
setnames(ids, names(ids), c('Hex', 'ShapeID'))

x<- read.xlsx(file.path(datapath, 'raw/OtterSpotterDatabyHexagonbyYear.xlsx'),
               startRow = 2, sheet='Sheet2')[,1:19]

setnames(x, 'Join_ID', 'Hex')

names(x)[2:3] <- paste0(names(x)[2:3],'.0')

dt <- data.table(x)

dtm <- melt(dt, id.vars='Hex')

dtm[,":="(Year=as.numeric(gsub('[^0-9]', '', variable))+2012,
          Type=gsub('[^a-zA-Z]', '', variable))]

dtm[is.na(value), value:=0]

spot <- dcast(dtm, Hex + Year ~ Type, value.var = 'value')
spotid <- merge(spot, ids, by='Hex')

fwrite(spotid, file.path(datapath, 'clean/OtterSpotter2020.csv'))

reports<- spotid[,.(Reports=sum(Reports)), by=Hex]
reporthex <- reports[Reports>=10, Hex]

spothex0 <- spotid[Hex %in% reporthex]

hexes <- sort(unique(spothex0$Hex))

hexids <- data.table(HexID=1:length(hexes),
                     Hex=hexes)

spothex <- merge(spothex0, hexids, by='Hex')

fwrite(spothex, file.path(datapath, 'clean/OtterSpotter2020Filtered.csv'))


