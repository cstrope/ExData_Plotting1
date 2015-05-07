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

#####
## Despite the fact that each of these plots has essentially the same data, and could thus
## all be resident in a single data frame, the sequence of exercises in Course Project 1
## caused me to build each data.frame. In any case, the names are more descriptive for me.
#####
plot.ll <- data.frame(ESM1 = data.for.plots$Sub_metering_1,		## Plot.lowerleft
                      ESM2 = data.for.plots$Sub_metering_2,
                      ESM3 = data.for.plots$Sub_metering_3,
					  day = day,
					  day.name = day.name,
					  total.seconds = total.seconds,
					  Date = data.for.plots$Date
					 )

plot.ul <- data.frame(Global_active_power = data.for.plots$Global_active_power,		## Plot.upperleft
					  day = day,
					  day.name = day.name,
					  total.seconds = total.seconds,
					  Date = data.for.plots$Date
					 )

plot.ur <- data.frame(voltage=data.for.plots$Voltage,
                                             day = day,
                                             day.name = day.name,
                                             total.seconds = total.seconds,
                                             Date = data.for.plots$Date
                                            )

plot.lr <- data.frame(GRP=data.for.plots$Global_reactive_power,
                                             day = day,
                                             day.name = day.name,
                                             total.seconds = total.seconds,
                                             Date = data.for.plots$Date
)


png("plot4.png", width=480, height=480)
par(mfrow=c(2,2))

plot(plot.ul$total.seconds, 					## Total seconds from 00:00:00 on Feb. 1, 2007 to 23:59:59 Feb 2, 2007
	 plot.ul$Global_active_power, 				## Data
	 type = "l", 								## Line plot
	 xaxt="n", 									## No axis ticks
	 xlab="", 									## Kill the x label
	 ylab="Global Active Power"   				## Reset y label
	)
axis(1,											## x-axis
	 at=ddays((min(plot.ul$day)-1):max(plot.ul$day)), 		## 0:2 (min(day)=1, -1 = 0; max(day)=2)
	 ## min(date)=Thursday. Max(Date) = Friday. Want Saturday as a tick for the very end, so +1. Label=T changes 5:7 to names.
	 labels=wday(seq(min(wday(plot.ul$Date)), max(wday(plot.ul$Date))+1, by=1), label=T) 
	)

plot(plot.ur$total.seconds, plot.ur$voltage, type='l', xaxt="n", xlab="datetime", ylab="Voltage")
axis(1,    										## x-axis
     at=ddays((min(plot.ur$day)-1):max(plot.ur$day)), 		## 0:2 (min(day)=1, -1 = 0; max(day)=2)
     ## min(date)=Thursday. Max(Date) = Friday. Want Saturday as a tick for the very end, so +1. Label=T changes 5:7 to names.
     labels=wday(seq(min(wday(plot.ur$Date)), max(wday(plot.ur$Date))+1, by=1), label=T) 
)

plot(plot.ll$total.seconds, plot.ll$ESM1, type='l', xaxt="n", xlab="", ylab="Energy sub metering")
with(plot.ll, lines(total.seconds, ESM3, col="blue"))		## Add to plot
with(plot.ll, lines(total.seconds, ESM2, col="red"))		## Add more to plot.
axis(1,    										## x-axis
     at=ddays((min(plot.ll$day)-1):max(plot.ll$day)), 		## 0:2 (min(day)=1, -1 = 0; max(day)=2)
     ## min(date)=Thursday. Max(Date) = Friday. Want Saturday as a tick for the very end, so +1. Label=T changes 5:7 to names.
     labels=wday(seq(min(wday(plot.ll$Date)), max(wday(plot.ll$Date))+1, by=1), label=T) 
)
legend("topright", 														## Location
	   lty=c(1,1,1), 													## Puts lines in the legend
	   col = c("black", "red", "blue"), 								## Line colors
	   legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),	## Labels legend values.
	   bty = "n"														## Remove box from legend.
	  )

plot(plot.lr$total.seconds, plot.lr$GRP, type='l', xaxt="n", xlab="datetime", ylab="Global_reactive_power")
axis(1,    										## x-axis
    at=ddays((min(plot.lr$day)-1):max(plot.lr$day)), 		## 0:2 (min(day)=1, -1 = 0; max(day)=2)
     ## min(date)=Thursday. Max(Date) = Friday. Want Saturday as a tick for the very end, so +1. Label=T changes 5:7 to names.
     labels=wday(seq(min(wday(plot.lr$Date)), max(wday(plot.lr$Date))+1, by=1), label=T) 
)
dev.off()
