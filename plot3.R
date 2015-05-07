library(lubridate)

mydata <- read.table("household_power_consumption.txt", sep=";", na.strings="?", stringsAsFactors=FALSE, header=TRUE)

## Lubridate change to POSIXct date. ##
mydata$Date <- dmy(mydata$Date)

## Subset for feb 1-2 of 2007
data.for.plots <- mydata[which(mydata$Date %in% seq(dmy("01022007"), dmy("02022007"), by="days")),]

## Get day
day <- day(data.for.plots$Date)
data.plot2 <- cbind(data.for.plots, day)

## Get the day name for axis labels.
day.name <- wday(data.for.plots$Date, label=T, abbr=T)
data.plot2 <- cbind(data.plot2, day.name)

data.plot2$Time <- hms(data.plot2$Time)

## Calculate the total seconds over the time frame:
total.seconds <- ddays(data.plot2$day-1) + 
                 dhours(hour(data.plot2$Time)) + 
                 dminutes(minute(data.plot2$Time)) + 
                 dseconds(data.plot2$Time) 
data.plot2 <- cbind(data.plot2, total.seconds)

plot.df <- data.frame(ESM1 = data.for.plots$Sub_metering_1,
                      ESM2 = data.for.plots$Sub_metering_2,
                      ESM3 = data.for.plots$Sub_metering_3,
					  day = day,
					  day.name = day.name,
					  total.seconds = total.seconds,
					  Date = data.for.plots$Date
					 )


png("plot3.png", width=480, height=480)
plot(plot.df$total.seconds, plot.df$ESM1, type='l', xaxt="n", xlab="", ylab="Energy sub metering")
with(plot.df, lines(total.seconds, ESM3, col="blue"))		## Add to plot
with(plot.df, lines(total.seconds, ESM2, col="red"))		## Add more to plot.
axis(1,    										## x-axis
     at=ddays((min(plot.df$day)-1):max(plot.df$day)), 		## 0:2 (min(day)=1, -1 = 0; max(day)=2)
     ## min(date)=Thursday. Max(Date) = Friday. Want Saturday as a tick for the very end, so +1. Label=T changes 5:7 to names.
     labels=wday(seq(min(wday(plot.df$Date)), max(wday(plot.df$Date))+1, by=1), label=T) 
)
legend("topright", 														## Location
	   lty=c(1,1,1), 													## Puts lines in the legend
	   col = c("black", "red", "blue"), 								## Line colors
	   legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")	## Labels legend values.
	  )
dev.off()
