datapath <- '/Users/echellwig/Google Drive/OtherPeople/otterData'

library(openxlsx)
library(data.table)
library(ggplot2)

x<- read.xlsx(file.path(datapath, 'raw/OtterSpotterDatabyHexagonbyYear.xlsx'),
               startRow = 2, sheet='Sheet2')[,1:19]

names(x)[2:3] <- paste0(names(x)[2:3],'.0')

dt <- data.table(x)

dtm <- melt(dt, id.vars='Join_ID')

dtm[,":="(Year=as.numeric(gsub('[^0-9]', '', variable))+2012,
          Type=gsub('[^a-zA-Z]', '', variable))]

dtm[is.na(value), value:=0]

spot0 <- dcast(dtm, Join_ID + Year ~ Type, value.var = 'value')

spot <- spot0[Reports>0]

spottab <- data.table(table(spot[,.(Reports, MaxOtters)]))
spottab <- spottab[N>0]
spottab[,":="(Reports=as.numeric(Reports), MaxOtters=as.numeric(MaxOtters))]

spotmod10 <- lm(MaxOtters ~ 1, data=spot)
spotmod11 <- lm(MaxOtters ~ Reports, data=spot)
spotmod12 <- lm(MaxOtters ~ Reports + Year, data=spot)


spotmod20 <- glm(MaxOtters ~ 1, data=spot, family=poisson)
spotmod21 <- glm(MaxOtters ~ Reports, data=spot, family=poisson)

new <- data.frame(Reports=1:45)

pdt <- data.table(Reports=1:45,
                  Null=predict(spotmod10, newdata=new),
                  Linear=predict(spotmod11, newdata=new),
                  Pois=predict.glm(spotmod21, newdata=new, type='response'))


pdtm <- melt(pdt, id.vars=c('Reports'), value.name='MaxOtters', 
             variable.name='Model')

splot <- ggplot(data=pdtm) +
    geom_line(aes(x=Reports, y=MaxOtters, group=Model, color=Model, linetype=Model)) +
    geom_point(data=spottab, aes(x=Reports, y=MaxOtters)) +
    theme_bw()





