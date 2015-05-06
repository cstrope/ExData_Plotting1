library(lubridate)

mydata <- read.table("household_power_consumption.txt", sep=";", na.strings="?", stringsAsFactors=FALSE, header=TRUE)

## Lubridate change to POSIXct date. ##
mydata$Date <- dmy(mydata$Date)

## Subset for feb 1-2 of 2007
data.for.plots <- mydata[which(mydata$Date %in% seq(dmy("01022007"), dmy("02022007"), by="days")),]

png("plot1.png", width=480, height=480)
hist(data.for.plots$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
dev.off()
