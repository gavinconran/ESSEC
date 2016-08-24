# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

# Let's load our dataset and call it data
data=read.table('day.csv',sep=',',header=TRUE) # The function read.table enables us to read flat files such as .csv files
data$dteday = as.Date(data$dteday, "%Y-%m-%d")

# Now let's have a look at our variables and see some summary statistics
str(data) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(data) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles


hist(data$mnth) # Produce a histogram of the credit scores
cor(data[,c(3:17)]) # Compute the correlation between all the numerical variables of the sample

linreg=lm(cnt~. ,data=data) # Estimate a linear regression model of Rating as a function of everything else.

cor(linreg$fitted.values,data$cnt) # Computes the correlation between the fitted values and the actual ones
plot(data$cnt,linreg$fitted.values) # Plot the fitted values vs. the actual ones

summary(linreg) # Reports the results of the regression
plot(data$mnth, data$cnt) # Allows to visualize the relationship between humidity and count

# An aggregated plot: humidity
tempdata=data
aggbTimeRank=aggregate(cnt~ hum, data=tempdata, FUN=mean) # We compute the average attrition rate for each value of TIC
plot(aggbTimeRank$hum,aggbTimeRank$cnt,main= "Bike use", ylab="Count", xlab= "humidity")

# An aggregated plot: weather situation
tempdata=data
aggbTimeRank=aggregate(cnt~ weathersit, data=tempdata, FUN=mean) # We compute the average attrition rate for each value of TIC
plot(aggbTimeRank$weathersit,aggbTimeRank$cnt,main= "Bike use", ylab="Count", xlab= "Weather Situation")

# An aggregated plot: week day
tempdata=data
aggbTimeRank=aggregate(cnt~ weekday, data=tempdata, FUN=mean) # We compute the average attrition rate for each value of TIC
plot(aggbTimeRank$weekday,aggbTimeRank$cnt,main= "Bike use", ylab="Count", xlab= "Week day")

# An aggregated plot: season
tempdata=data
aggbTimeRank=aggregate(cnt~ season, data=tempdata, FUN=mean) # We compute the average attrition rate for each value of TIC
plot(aggbTimeRank$season,aggbTimeRank$cnt,main= "Bike use", ylab="Count", xlab= "season")

# An aggregated plot: month
tempdata=data
aggbTimeRank=aggregate(cnt~ mnth, data=tempdata, FUN=mean) # We compute the average attrition rate for each value of TIC
plot(aggbTimeRank$mnth,aggbTimeRank$cnt,main= "Bike use by month", ylab="Count", xlab= "month", type='l')
plot(aggbTimeRank$mnth,aggbTimeRank$cnt,main= "Bike use by month", ylab="Count", xlab= "month", type='l')
summary(aggbTimeRank)

# Simple Horizontal Bar Plot with Added Labels
# Fitting Labels
par(las=1) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
plot <- barplot(rev(aggbTimeRank$cnt), main="Average Bike Rentals by Month", horiz=TRUE,
        names.arg=rev(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sept", "Oct", "Nov", "Dec")), 
        ylab="Month", xlab= "Average Bike Rental",
        col=c("darkblue"),
        xlim=c(0,max(aggbTimeRank$cnt)*1.2),
        cex.names=0.8)

text(x= rev(aggbTimeRank$cnt)+300, col=c("blue"), y= plot, labels=as.character(as.integer(rev(aggbTimeRank$cnt))))

# Seasonality
# An aggregated plot: instant (time)
plot(data$instant,data$cnt,main="Count over time",xlab="Time (in month)",ylab="Monthly count")
regres=lm(cnt~temp,data=data) # Build a linear regression model
summary(regres)

# Recovery thanks to the model:
plot(data$instant, data$cnt,main="Bike count over time",xlab="Time (in month)",ylab="Monthly Count",ylim=c(0,max(data$cnt)*1.2),type='l')
lines(data$instant,regres$fitted.values,type='l',col='blue',lty=2)
legend("topleft",c("Actual count","Count by the model"),lty=c(1,2),col=c('black','blue'))
