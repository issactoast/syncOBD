# Load 'jsonlite' package to read JSON files.
library(jsonlite)


# read obd2 trip & mobile trip
obd2_data <- fromJSON("./obd2_trips.json", flatten = TRUE)
mobile_data <- fromJSON("./mobile_trips.json", flatten = TRUE)

length(obd2_data)
length(mobile_data)

# determine the conversion factor 3.6
# 3.6 vs. 3.570348

# obd2 unit m/s
sample1 <- obd2_data[[1]]

# mobile speed unit km/h
mobilesample1 <- mobile_data[[1]]

# conversion factor
max(sample1$speed) / max(mobilesample1$speed) 

# Visual confirmation
with(sample1, plot(timestamp, speed/3.570348, type = "l"))
with(mobilesample1, points(timestamp - timestamp[1]  + 270,
                           speed, col = "red",
                           type = "l"))


y <- sample1$speed / 3.570348
x <- mobilesample1$speed 
plot(sample1$timestamp, y, type = "l")
mobilesample1$timestamp[256] - mobilesample1$timestamp[1]

points(1:length(x) + sample1$timestamp[256], x, type = "l", col = "red")

result <- rep(0, length(y))
for (i in 1:length(y)){
    n2 <- min((i+(length(x)-1)), length(y))
    result[i] <- sqrt(sum((y[i:n2] - x[1:(n2-(i-1))])^2)) / (n2-(i-1))
    # result[i] <- sum(abs(y[i:n2] - x[1:(n2-(i-1))])) / (n2-(i-1))

}

plot(1:length(result), result)
start_time <- sample1$timestamp[which.min(result[1:600])] 

plot(sample1$timestamp, y, type = "l")
points(mobilesample1$timestamp - mobilesample1$timestamp[1] + start_time + 10,
       x, col = "red", type = "l")

sample1$timestamp

# make one dataframe
obd2_data <- Reduce(rbind, obd2_data)
mobile_data <- Reduce(rbind, mobile_data)

length(unique(obd2_data$trip_id))
dim(mobile_data)

# Print the result.
print(result)

# load data
mydata2 <- read.csv("https://raw.githubusercontent.com/issactoast/EnBlog/master/static/files/stat2010/examscore2.csv", header = TRUE)

# check the result
head(mydata2)


model1 <- lm(final ~ midterm, data = mydata2)

summary(model1)
with(mydata2, cor(final, midterm))^2
0.677^2
