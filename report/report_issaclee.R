SimilarityMeasure <- function(obd_trip_data, mobile_trip_data){
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
    }

    # find the arg min of i
    # min_i <- which.min(result)
    
    result # [min_i]
}

SimilarityMeasure_min <- function(obd_trip_data, mobile_trip_data){
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
    result <- rep(1000, n_total-1)
    
    # sliding window from 1st element of reference speed vector
    # to the last element of reference speed vector.
    
    for (k in 1:(n_total-1)){
        ref_sub <- ref_speed[max(k-n_2+1, 1):min(n_1,k)]
        tar_sub <- target_speed[max(n_2-k+1, 1):(n_2-max(0,k-n_1))]
        
        # calculating the overlapped length
        n_overlap <- min(n_1, k) - (max(k-n_2, 1) - 1)
        
        # difference measure
        result[k] <- sqrt(sum((ref_sub - tar_sub)^2)) / n_overlap  +
            + 10 * (1 / (n_overlap))
        
    }

    min(result)
}

# Find the best OBD trip using sync_trip2 function 
FindBestTrip <- function(mobile_trip, obd2_data){
    # Apply sync_trip2 to the all obd2 trip
    match_result <- lapply(obd2_data, sync_trip2,
                           mobile_trip_data = mobile_trip)

    # Make the result into the matrix form
    match_result <- matrix(unlist(match_result),
                           ncol = 5, byrow = TRUE)
    trip_fit <- match_result[,4]
    best_trip <- which.min(trip_fit)
    # Final result as a list
    list(start_index_ref = match_result[best_trip, 1],
         start_index_tar = match_result[best_trip, 2],
         overlap_length = match_result[best_trip, 3],
         dissimilarity = match_result[best_trip, 4],
         # k = match_result[best_trip, 5],
         macthed_trip = best_trip)
}

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
        result[k] <- sqrt(sum((ref_sub - tar_sub)^2)) / n_overlap + (1 / (n_overlap/100))
        # result[k] <- sqrt(sum((ref_sub - tar_sub)^2)) / n_overlap + n_2 / n_overlap
        start_p_ref[k] <- max(k-n_2+1, 1)
        start_p_tar[k] <- max(n_2-k+1, 1)
        n_overlap_vec[k] <- n_overlap
    }
    
    # find the arg min of i
    min_i <- which.min(result)
    
    # return the result; min i & corresponding n2
    data.frame(start_index_ref = start_p_ref[min_i],
               start_index_tar = start_p_tar[min_i],
               overlap_length = n_overlap_vec[min_i],
               dissimilarity = result[min_i],
               k = min_i)
}

# make function 2: visualization
VisTrip <- function(target_trip_data, obd2_data, match_info){
    
    ref_trip_data <- obd2_data[[match_info$macthed_trip]]
    
    # grab information about the start and end points
    start_p_ref <- match_info$start_index_ref
    end_p_ref <- start_p_ref + match_info$overlap_length - 1
    
    start_p_tar <- match_info$start_index_tar
    end_p_tar <- start_p_tar + match_info$overlap_length - 1
    # k <- match_info$k
    
    mydata <- data.frame(timestamp = ref_trip_data$timestamp[start_p_ref:end_p_ref],
                         speed = target_trip_data$speed[start_p_tar:end_p_tar])
    
    # ref trip data, trip_data both have timestamp, speed
    ggplot2::ggplot(data = ref_trip_data, aes(x = timestamp,
                                              y = speed / 3.6)) +
        ggplot2::geom_line()+
        ggplot2::geom_line(data = mydata, aes(timestamp, speed),
                           col = "red") +
        ggplot2::labs(x = "Time (sec.)",
                      y = "Speed (km/h)") +
        ggplot2::theme(legend.position="none") +
        ggplot2::theme_bw()
}
