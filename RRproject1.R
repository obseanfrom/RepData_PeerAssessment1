##Read data
library(dplyr)
library(ggplot2)
stepmean <- NULL
stepmedian <- NULL
data <- na.omit((data.frame(read.csv("activity.csv"))))

##Process data

data$date <- as.Date(data$date)
datatemp <- aggregate(steps ~ date, data, sum)

##Create histogram

histplot <- ggplot(datatemp, aes(x=steps)) +
  geom_histogram(binwidth = 750) 

print(histplot)

##Find mean and Median of steps taken each day

datasummary <- summarise(datatemp, stepmean = mean(datatemp$steps),
                      stepmedian = median(datatemp$steps))
print(datasummary)

##Time series plot of the average number of steps taken

tsplot <- ggplot(datatemp, aes(x = date, y = steps)) +
  geom_line()

print(tsplot)

##Find the 5 minute interval that contains the maximum number of steps on average

intdata <- data[,c("steps","interval")]
intdata$interval <- as.factor(intdata$interval)
intdatam <- as.data.frame(with(intdata, tapply(steps, interval, mean)))
intdata <- cbind("Interval" = rownames(intdatam),intdatam)
rownames(intdata) <- 1:nrow(intdata)
colnames(intdata) <- c("Interval","Ave_Steps")
intdatamax <- intdata[order(intdata$Ave_Steps, decreasing = T),][1:10,]

max <- intdatamax[1,]

##Describe a strategy to imput missing values
##This appears best done by averaging data for each interval across each day
##then the resultant data can be used to fill in any NA values with that interval's
##average value

dataunc <- data.frame(read.csv("activity.csv"))
dataunc$date <- as.Date(dataunc$date)
NAindex <- which(is.na(dataunc$steps))
for(i in NAindex) {
    interval <- dataunc$interval[i]
    dataunc$steps[i] <- intdata[intdata$Interval ==interval,]$Ave_Steps
}
##Histplot after fill

histplotfilled <- ggplot(dataunc, aes(x=steps)) +
  geom_histogram(binwidth = 50) 

print(histplotfilled)

##this doesn't seem to have worked quite right

##Panel plot comparing steps during a weekend vs a weekday:

