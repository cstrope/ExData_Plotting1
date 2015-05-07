library(lubridate)

mydata <- read.table("household_power_consumption.txt", sep=";", na.strings="?", stringsAsFactors=FALSE, header=TRUE)

## Lubridate change to POSIXct date. ##
mydata$Date <- dmy(mydata$Date)

## Subset for feb 1-2 of 2007
data.for.plots <- mydata[which(mydata$Date %in% seq(dmy("01022007"), dmy("02022007"), by="days")),]

## Get day
day <- day(data.for.plots$Date)

## Get the day name for axis labels.
day.name <- wday(data.for.plots$Date, label=T, abbr=T)

## Calculate the total seconds over the time frame:
total.seconds <- ddays(data.plot2$day-1) + 
                 dhours(hour(data.plot2$Time)) + 
                 dminutes(minute(data.plot2$Time)) + 
                 dseconds(data.plot2$Time) 

plot.df <- data.frame(Global_active_power = data.for.plots$Global_active_power,
					  day = day,
					  day.name = day.name,
					  total.seconds = total.seconds,
					  Date = data.for.plots$Date
					 )

png("plot2.png", width=480, height=480)
plot(plot.df$total.seconds, 					## Total seconds from 00:00:00 on Feb. 1, 2007 to 23:59:59 Feb 2, 2007
	 plot.df$Global_active_power, 				## Data
	 type = "l", 								## Line plot
	 xaxt="n", 									## No axis ticks
	 xlab="", 									## Kill the x label
	 ylab="Global Active Power (kilowatts)"   	## Reset y label
	)
axis(1,											## x-axis
	 at=ddays((min(plot.df$day)-1):max(plot.df$day)), 		## 0:2 (min(day)=1, -1 = 0; max(day)=2)
	 ## min(date)=Thursday. Max(Date) = Friday. Want Saturday as a tick for the very end, so +1. Label=T changes 5:7 to names.
	 labels=wday(seq(min(wday(plot.df$Date)), max(wday(plot.df$Date))+1, by=1), label=T) 
	)
dev.off()
