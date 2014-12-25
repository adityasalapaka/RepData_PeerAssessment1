#Working directory should have activity.csv in root.

data <- read.csv("activity.csv")

x <- aggregate(data$steps, by = list(Date = data$date), FUN = sum)

hist(x$x, main = paste("Histogram of Total Number of Steps Taken Per Day"),
     xlab = "Number of Steps")

mean(x$x, na.rm = TRUE)
median(x$x, na.rm = TRUE)

data2 <- data[with(data, order(interval)),]
data2 <- data2[complete.cases(data2),]

y <- aggregate(data2$steps[complete.cases(data2$steps)], 
               by = list(interval = data2$interval), FUN = mean)

plot(y, type = "l", xlab = "Time Interval", ylab = "Number of Steps", 
     main = paste("Average of Number of Steps Taken Per Day By Interval"))

y$interval[which(y$x == max(y$x))]

sum(!complete.cases(data))

library("plyr")

data3 <- data[with(data, order(interval)),]
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
data3 <- ddply(data3, ~ interval, transform, steps = impute.mean(steps))
data3 <- data3[order(data3$interval),]

z <- aggregate(data3$steps, by = list(Date = data3$date), FUN = sum)
hist(z$x, main = paste("Histogram of Total Number of Steps Taken Per Day"),
     xlab = "Number of Steps")

mean(z$x, na.rm = TRUE)
median(z$x, na.rm = TRUE)

weekend <- c("Saturday", "Sunday")

data3$daytype = as.factor(ifelse(is.element(weekdays(as.Date(data3$date))
                                            ,weekend), "Weekend", "Weekday"))

stepsbyday <- aggregate(steps ~ interval + daytype, data3, mean)

library("ggplot2")
g <- ggplot(stepsbyday, aes(interval, steps))
p <- g + geom_line() + facet_grid(daytype ~.) + xlab("Time Interval")
p <- p + ylab("Number of Steps")
print(p)