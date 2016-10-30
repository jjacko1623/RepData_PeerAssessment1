##Reproducible Research

##1
#set up directory and data
setwd("C:/Users/Sophy/Desktop/JJ Coursera/Reproducible Research/RepData_PeerAssessment1")
#unzip("activity.zip")
activityData <- read.csv("activity.csv",header=T)
totalStepsperdayData <- aggregate(activityData$steps~activityData$date,activityData,FUN = sum, na.action = na.omit)

##2
hist(totalStepsperdayData[,2], xlab = "Number of Steps per day", main = "Histogram of Steps", breaks = 10)

##3
#print results
print(paste("The mean total number of steps per day is", round(mean(totalStepsperdayData[,2]),digits = 2)))
print(paste("The median total number of steps per day is", round(median(totalStepsperdayData[,2]),digits=2)))

##4
averageStepsperintData <- aggregate(activityData$steps~activityData$interval,activityData,FUN = mean,na.action = na.omit)
plot(averageStepsperintData[,1],averageStepsperintData[,2],xlab="Interval (start times)",ylab="Average Steps", main="Average Steps per interval (excl NA data)",type="l")

##5
rowIntMaxStep <- averageStepsperintData[which.max(averageStepsperintData[,2]),]
print(paste("Starting time of max average steps (5min interval) is:", rowIntMaxStep[1]))

##6
numNAs <- sum(is.na(activityData$steps))
#replace NAs with daily average, this is very simplistic and not realistic
averageSteps <- mean(averageStepsperintData[,2])
activityData[is.na(activityData)] <- averageSteps

noNAtotalStepsperdayData <- aggregate(activityData$steps~activityData$date,activityData,FUN = sum, na.action = na.omit)

##7
hist(noNAtotalStepsperdayData[,2], xlab = "Number of Steps per day", main = "Histogram of Steps (replaced NAs)", breaks = 10)

#print results
print(paste("The mean total number of steps per day is", round(mean(noNAtotalStepsperdayData[,2]),digits = 2)))
print(paste("The median total number of steps per day is", round(median(noNAtotalStepsperdayData[,2]),digits = 2)))

##The effect
#My method of replacing the NA values with the average accross all intervals has had the typical effect
#The mean and median are now the same as I've added more values at the middle
#This is shown in the hist plots as all frequencies remain the same, except for the middle bar
#which has now been increased by the replacement of the NA values being assigned the middle value

##8
#make weekday data
#leave NA values out, as in original
activityData$day <- weekdays(as.Date(activityData$date,'%Y-%m-%d'))
weekdaysref <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activityData$wday <- factor((activityData$day %in% weekdaysref), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

weekdayData <- activityData[activityData$wday == "weekday",]
weekendData <- activityData[activityData$wday == "weekend",]

averageStepsperintWeekdayData <- aggregate(weekdayData$steps~weekdayData$interval,weekdayData,FUN = mean,na.action = na.omit)
averageStepsperintWeekendData <- aggregate(weekendData$steps~weekendData$interval,weekendData,FUN = mean,na.action = na.omit)

par(mfrow = c(2,1))
plot(averageStepsperintWeekdayData[,1],averageStepsperintWeekdayData[,2],main="Weekday (excl NA data)",type="l",ylab="",ylim = c(0,200),xlab = "")
plot(averageStepsperintWeekendData[,1],averageStepsperintWeekendData[,2],xlab="Interval (start times)", main="Weekend (excl NA data)",type="l",ylab="",ylim = c(0,200))
