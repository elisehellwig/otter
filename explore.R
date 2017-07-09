datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)

otr <- read.csv(file.path(datapath, 'otterclean.csv'))

################################################################

countindex <- which(count(otr$Area)$freq==5)

allyrnames <- count(otr$Area)[countindex,'x']

allyears <- otr[otr$Area %in% allyrnames, ]

totals <- sapply(unique(otr$Year), function(y) {
    sum(allyears[allyears$Year==y, 'Pop'])
})

tdf <- data.frame(year=unique(otr$Year), pop=totals)
