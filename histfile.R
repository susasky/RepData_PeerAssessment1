data <- read.table("activity.csv",
                    header = TRUE,
                    sep = ",")
tot<-tapply(data$steps, data$date, sum)
AvOnInt<-tapply(data$steps, data$interval, mean,na.rm=TRUE)
data$avonint<-AvOnInt
data["steps"][is.na(data["steps"])]<-data["avonint"][is.na(data["steps"])]
newdata<-data[,1:3]
newdata$steps<-as.integer(newdata$steps)
newtot<-tapply(newdata$steps, newdata$date, sum)
Sys.setlocale("LC_TIME", "C")
#Adding a column to the dataframe that takes into account the day of the week
newdata$weekdays<-weekdays(as.Date(newdata$date),abbreviate=TRUE)
#adding a new column to store the weekend factor
newdata$isweekend<-"temp"
#setting the "non weekend" values
newdata["isweekend"][newdata["weekdays"]!="Sun" | newdata["weekdays"]!="Sat"]<-"weekday"
#setting the weekend values
newdata["isweekend"][newdata["weekdays"]=="Sun"|newdata["weekdays"]=="Sat"]<-"weekend"
#cohercing the isweekend variable to factors
newdata$isweekend<-as.factor(newdata$isweekend)
str(newdata)
valswd<-tapply(newdata$steps[newdata$isweekend=="weekday"],
newdata$interval[newdata$isweekend=="weekday"],mean)
valswd
valswe<-tapply(newdata$steps[newdata$isweekend=="weekend"],
newdata$interval[newdata$isweekend=="weekend"],mean)
avdata<-rbind(data.frame("meansteps"=as.vector(valswd),
"interval"=names(valswd),"isweekend"="weekday"),
data.frame("meansteps"=as.vector(valswe),
"interval"=names(valswe),"isweekend"="weekend"))
avdata
str(avdata)
xyplot(avdata$meansteps ~ avdata$interval | avdata$isweekend, avdata, c(1,2))
library(lattice)
xyplot(avdata$meansteps ~ avdata$interval | avdata$isweekend, avdata, c(1,2))
xyplot(avdata$meansteps ~ avdata$interval | avdata$isweekend, avdata, c(2,1))
xyplot(avdata$meansteps ~ avdata$interval | avdata$isweekend, avdata)
xyplot(avdata$meansteps ~ avdata$interval | avdata$isweekend, avdata)
savehistory(file="histfile.R")
