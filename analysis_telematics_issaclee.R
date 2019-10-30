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
with(sample1, plot(timestamp, speed/3.6, type = "l"))
with(mobilesample1, points(timestamp - timestamp[1]  + 270,
                           speed, col = "red",
                           type = "l"))


y <- sample1$speed / 3.6
x <- mobilesample1$speed 
plot(sample1$timestamp, y, type = "l")
mobilesample1$timestamp[256] - mobilesample1$timestamp[1]

points(1:length(x) + sample1$timestamp[256], x, type = "l", col = "red")
i <- 256
result <- rep(0, length(y))
for (i in 1:length(y)){
    n2 <- min((i+(length(x)-1)), length(y))
    result[i] <- sqrt(sum((y[i:n2] - x[1:(n2-(i-1))])^2)) / (n2-(i-1))
    # result[i] <- sqrt(mean((y[i:n2] - x[1:(n2-(i-1))])^2))
    # result[i] <- sum(abs(y[i:n2] - x[1:(n2-(i-1))])) / (n2-(i-1))
}

plot(1:length(result), result, type = "l")
start_index <- which.min(result) 

# result checking
plot(sample1$timestamp, y, type = "l")
points(sample1$timestamp[start_index:n2],
       x[1:(n2-(start_index-1))], col = "red", type = "l")

# make function: method 1
sync_trip <- function(obd_trip_data, mobile_trip_data){
    # load reference speed
    # sync scale using division of 3.6
    ref_speed <- obd_trip_data$speed / 3.6
    
    # load mobile speed vector
    x <- mobile_trip_data$speed
    
    # measure length of the speed vector
    n_ref <- length(ref_speed)
    n_x <- length(x)
    
    # make result vector
    result <- rep(0, n_ref)
    n2_vec <- rep(0, n_ref)
    
    # sliding window from 1st element of reference speed vector
    # to the last element of reference speed vector.
    for (i in 1:n_ref){
        # calculating the overlapped length
        n2 <- min( (i+(n_x-1)), n_ref ) 
        # (n2-(i-1)) data points used
        result[i] <- sqrt(sum((ref_speed[i:n2] - x[1:(n2-(i-1))])^2)) / (n2-(i-1))
        n2_vec[i] <- n2
    }
    
    # find the arg min of i
    min_i <- which.min(result)
    
    # return the result; min i & corresponding n2
    list(start_index = min_i,
         end_index = n2_vec[min_i],
         fitness = result[min_i])
}

obd_trip_data <- sample1
mobile_trip_data <- mobilesample1

# mobile speed unit km/h
mobilesample1 <- mobile_data[[1]]

# find best OBD2 trip sync_trip
match_result <- lapply(obd2_data, sync_trip,
                       mobile_trip_data = mobilesample1)
match_result <- matrix(unlist(match_result), ncol = 3, byrow = TRUE)
trip_fit <- match_result[,3]
plot(1:length(trip_fit), trip_fit, type = "l")
match_info <- c(match_result[which.min(trip_fit),],
                which.min(trip_fit))

# visualization
vis_trip(obd2_data[[match_info[4]]], mobile_data[[1]], match_info)

vis_trip <- function(ref_trip_data, trip_data, match_info){
    # grab information about the start and end points
    start_p <- match_info[1]
    end_p <- match_info[2]
    # ref trip data, trip_data both have timestamp, speed
    with(ref_trip_data,
         plot(timestamp, speed/3.6, type = "l")
    )
    with(trip_data,
         points(ref_trip_data$timestamp[start_p:end_p],
                speed[1:(end_p-(start_p-1))],
                col = "red", type = "l")
    )
}


# make function: method 2
sync_trip2 <- function(obd_trip_data, mobile_trip_data){
    # load reference speed
    # sync scale using division of 3.6
    ref_speed <- obd_trip_data$speed / 3.6
    
    # load mobile speed vector
    target_speed <- mobile_trip_data$speed
    
    # measure length of the speed vector
    n_1 <- length(ref_speed)
    n_2 <- length(target_speed)
    n_total <- n_1 + n_2
    
    # make result vector
    result <- rep(1000, n_total)
    start_p_ref <- rep(0, n_total)
    start_p_tar <- rep(0, n_total)
    n_overlap_vec <- rep(0, n_total)

    
    # sliding window from 1st element of reference speed vector
    # to the last element of reference speed vector.
    for (k in 1:n_total){
        ref_sub <- ref_speed[max(k-n_2+1, 1):min(n_1,k)]
        tar_sub <- target_speed[max(n_2-k+1, 1):(n_2-max(0,k-n_1))]

        # calculating the overlapped length
        n_overlap <- min(n_1, k) - (max(k-n_2, 1) - 1)
        
        # difference measure
        result[k] <- sqrt(sum((ref_sub - tar_sub)^2)) / n_overlap + (1 / (n_overlap/10))
        start_p_ref[k] <- max(k-n_2+1, 1)
        start_p_tar[k] <- max(n_2-k+1, 1)
        n_overlap_vec[k] <- n_overlap
    }
    
    # find the arg min of i
    min_i <- which.min(result)
    
    # return the result; min i & corresponding n2
    data.frame(start_index_ref = start_p_ref[min_i],
         start_index_tar = start_p_tar[min_i],
         n_overlap = n_overlap_vec[min_i],
         fitness = result[min_i],
         k = min_i)
}



# mobile speed unit km/h
mobilesample1 <- mobile_data[[4]]

# find best OBD2 trip using sync_trip2
match_result <- lapply(obd2_data, sync_trip2,
                       mobile_trip_data = mobilesample1)
match_result <- matrix(unlist(match_result), ncol = 5, byrow = TRUE)
trip_fit <- match_result[,4]
plot(1:length(trip_fit), trip_fit, type = "l")
match_info <- c(match_result[which.min(trip_fit),],
                which.min(trip_fit))

# make function 2: visualization
vis_trip2(obd2_data[[match_info[6]]], mobilesample1, match_info)

vis_trip2 <- function(ref_trip_data, target_trip_data, match_info){
    # grab information about the start and end points
    start_p_ref <- match_info[1]
    end_p_ref <- start_p_ref + match_info[3] - 1
    
    start_p_tar <- match_info[2]
    end_p_tar <- start_p_tar + match_info[3] - 1
    k <- match_info[5]
    # ref trip data, trip_data both have timestamp, speed
    with(ref_trip_data,
         plot(timestamp, speed/3.6, type = "l")
    )
    with(target_trip_data,
         points(ref_trip_data$timestamp[start_p_ref:end_p_ref],
                speed[start_p_tar:end_p_tar],
                col = "red", type = "l")
    )
}
    
# obd2 unit m/s
obd2_sample1 <- obd2_data[[35]]
myresult <- sync_trip(obd2_sample1, mobilesample1)

# result checking
plot(obd2_sample1$timestamp, obd2_sample1$speed/3.6, type = "l")
points(obd2_sample1$timestamp[myresult$start_index:myresult$end_index],
       x[1:(myresult$end_index-(myresult$start_index-1))], col = "red", type = "l")



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
