detect_anoms <- function(data, k = 0.49, alpha = 0.05, step = 1, num_obs_per_period = NULL,
                         use_decomp = TRUE, use_esd = FALSE, one_tail = TRUE,
                         upper_tail = TRUE, verbose = FALSE) {
  # Detects anomalies in a time series using S-H-ESD.
  #
  # Args:
  #	 data: Time series to perform anomaly detection on.
  #	 k: Maximum number of anomalies that S-H-ESD will detect as a percentage of the data.
  #	 alpha: The level of statistical significance with which to accept or reject anomalies.
  #  step: the step of down sample 
  #	 num_obs_per_period: Defines the number of observations in a single period, and used during seasonal decomposition.
  #	 use_decomp: Use seasonal decomposition during anomaly detection.
  #	 use_esd: Uses regular ESD instead of hybrid-ESD. Note hybrid-ESD is more statistically robust.
  #	 one_tail: If TRUE only positive or negative going anomalies are detected depending on if upper_tail is TRUE or FALSE.
  #	 upper_tail: If TRUE and one_tail is also TRUE, detect only positive going (right-tailed) anomalies. If FALSE and one_tail is TRUE, only detect negative (left-tailed) anomalies.
  #	 verbose: Additionally printing for debugging.
  # Returns:
  #   A list containing the anomalies (anoms) and decomposition components (stl).
  
  if(is.null(num_obs_per_period)){
    stop("must supply period length for time series decomposition")
  }
  
  num_obs <- nrow(data)
  
  # Check to make sure we have at least two periods worth of data for anomaly context
  if(num_obs < num_obs_per_period * 2){
    stop("Anom detection needs at least 2 periods worth of data")
  }
  
  # Check if our timestamps are posix
  posix_timestamp <- if (class(data[[1L]])[1L] == "POSIXlt") TRUE else FALSE
  
  # Handle NAs
  if (length(rle(is.na(c(NA,data[[2L]],NA)))$values)>3){
    stop("Data contains non-leading NAs. We suggest replacing NAs with interpolated values (see na.approx in Zoo package).")
  } else {
    data <- na.omit(data)
  }
  
  ###### use stl function
  ### use interpolation to solve this problem
  #time1 <- Sys.time();
  tmp_data <- approx(1:num_obs, data$count, xout = seq(1, num_obs, by = step))
  num_obs_per_period = round(num_obs_per_period / step)
  
  # -- Step 1: Decompose data. This returns a univarite remainder which will be used for anomaly detection. Optionally, we might NOT decompose.
#   data_decomp <- stl(ts(tmp_data[[2L]], frequency = num_obs_per_period),
#                      s.window = "periodic", robust = TRUE)        
#   
#   temp_trend = approx(data_decomp$time.series[,"trend"], method = "linear", n = num_obs)
#   temp_seasonal = approx(data_decomp$time.series[,"seasonal"], method = "linear", n = num_obs)
  # use stl2
  data_decomp <- stl2(ts(tmp_data[[2L]], frequency = num_obs_per_period),
                     s.window = "periodic", robust = TRUE)        
  
  temp_trend = approx(trend.stl2(data_decomp), method = "linear", n = num_obs)
  temp_seasonal = approx(seasonal.stl2(data_decomp), method = "linear", n = num_obs)
  
  ####################################
  # Remove the seasonal component, and the median of the data to create the univariate remainder
  data <- data.frame(timestamp = data[[1L]], count = (data[[2L]] - temp_seasonal$y - temp_trend$y - median(data[[2L]])))
  # Store the smoothed seasonal component, plus the trend component for use in determining the "expected values" option
  data_decomp <- data.frame(timestamp=data[[1L]], count=(as.numeric(trunc(temp_trend$y + temp_seasonal$y))))
  
  if(posix_timestamp){
    data_decomp <- format_timestamp(data_decomp)
  }
  # Maximum number of outliers that S-H-ESD can detect (e.g. 49% of data)
  max_outliers <- trunc(num_obs*k)
  
  if(max_outliers == 0){
    stop(paste0("With longterm=TRUE, AnomalyDetection splits the data into 2 week periods by default. You have ", num_obs, " observations in a period, which is too few. Set a higher piecewise_median_period_weeks."))
  }
  
  ## Define values and vectors.
  n <- length(data[[2L]])
  if (posix_timestamp){
    R_idx <- as.POSIXlt(data[[1L]][1L:max_outliers], tz = "UTC")
  } else {
    R_idx <- 1L:max_outliers
  }
  R_score <- vector(length = max_outliers)
  
  num_anoms <- 0L
  func_ma <- match.fun(median)
  func_sigma <- match.fun(mad)
  
  dataMean <- mean(data[[2L]])
  dataMedian <- func_ma(data[[2L]])
  dataStd <- sd(data[[2L]]) 
  
  # time1 <- Sys.time();
  ###### use generalized esd function 
  # protect against constant time series
  if(dataStd == 0) 
    break
  
  if(one_tail){
    if(upper_tail){
      ares <- data[[2L]] - dataMedian
    } else {
      ares <- dataMedian - data[[2L]]
    }
  } else {
    ares = abs(data[[2L]] - dataMedian)
  }
  ares <- ares/dataStd
  aresOrder <- order(-data[[2L]])
  
  medianIndex <- n/2
  left <- 1
  right <- n
  nowLength <- n
  for (i in 1L:max_outliers) {
    if(one_tail){
      p <- 1 - alpha/(n-i+1)
    } else {
      p <- 1 - alpha/(2*(n-i+1))
    }
    
    t <- qt(p,(n-i-1L))
    lambda_critical <- t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1))
    
    if (left >= right) break
    if (nowLength < 1) break
    # remove largest
    # remove the max diff   left or right
    if (abs(data[[2L]][aresOrder[left]] - dataMedian) > abs(data[[2L]][aresOrder[right]] - dataMedian)) {
      temp_max_idx <- aresOrder[left]
      left <- left + 1
      medianIndex <- medianIndex + 1
    }
    else {
      temp_max_idx <- aresOrder[right]
      right <- right - 1
      medianIndex <- medianIndex - 1
    }
    # get the R
    R <- abs((data[[2L]][temp_max_idx] - dataMedian) / dataStd)
    
    # recalculate the dataMean and dataStd
    # use math sd
    #dataStd <- sqrt((nowLength * (dataStd**2 + dataMean**2) - data[[2L]][temp_max_idx]**2 - (nowLength * dataMean - data[[2L]][temp_max_idx])**2/(nowLength - 1)) / (nowLength - 1))
    # use statics sd
    dataStd <- sqrt(((nowLength - 1) * (dataStd**2 + dataMean**2) - data[[2L]][temp_max_idx]**2 - ((nowLength - 1) * dataMean - data[[2L]][temp_max_idx])**2/(nowLength - 2)) / (nowLength - 2))
    dataMean <- (dataMean * nowLength - data[[2L]][temp_max_idx]) / (nowLength - 1)
    dataMedian <- data[[2L]][aresOrder[medianIndex]]
    nowLength <- nowLength - 1
    #record the inx
    R_idx[i] <- data[[1L]][temp_max_idx]
    R_score[i] <- R
    if (R < lambda_critical || is.nan(dataStd)) {
      break
    }
    #points(temp_max_idx, data[[2L]][temp_max_idx], col = "red")
    
    num_anoms <- i
  }
  if(num_anoms > 0) {
    R_idx <- R_idx[1L:num_anoms]
  } else {
    R_idx = NULL
  }
  #print(num_anoms)
  
  return(list(anoms = R_idx, anoms_score = R_score, stl = data_decomp))
}


AnomalyDetectionTs <- function(x, max_anoms = 0.10, num_period_in_part = 3, sample_step = 10, anoms_threshold = 1.05, down_sample_step = 60, direction = 'pos',
                               alpha = 0.05, only_last = NULL, threshold = 'None',
                               e_value = FALSE, longterm = FALSE, piecewise_median_period_weeks = 2, plot = FALSE,
                               y_log = FALSE, xlabel = '', ylabel = 'count',
                               title = NULL, verbose=FALSE, na.rm = FALSE){
  #' @name AnomalyDetectionTs
  #' @param x Time series as a two column data frame where the first column consists of the
  #' timestamps and the second column consists of the observations.
  #' @param max_anoms Maximum number of anomalies that S-H-ESD will detect as a percentage of the
  #' data.
  #' @param num_period_in_part  the numbers of the period in one input window
  #' @param sample_step   if the input data is too large, we should sample the data with sample_step
  #' @param anoms_threshold  use the threshold to filter the anoms. such as if anoms_threshold = 1.05, 
  #' then we will filter the anoms that exceed the exceptional critical value 100%-105%
  #' @param down_sample_step   the step of down_sample_step to decline the data size to STL decompostion,
  #' each period has a number of 20 is ok. so different data has different down_sample_step 
  #' @param direction Directionality of the anomalies to be detected. Options are:
  #' \code{'pos' | 'neg' | 'both'}.
  #' @param alpha The level of statistical significance with which to accept or reject anomalies.
  #' @param only_last Find and report anomalies only within the last day or hr in the time series.
  #' \code{NULL | 'day' | 'hr'}.
  #' @param threshold Only report positive going anoms above the threshold specified. Options are:
  #' \code{'None' | 'med_max' | 'p95' | 'p99'}.
  #' @param e_value Add an additional column to the anoms output containing the expected value.
  #' @param longterm Increase anom detection efficacy for time series that are greater than a month.
  #' See Details below.
  #' @param piecewise_median_period_weeks The piecewise median time window as described in Vallis, Hochenbaum, and Kejariwal (2014).
  #' Defaults to 2.
  #' @param plot A flag indicating if a plot with both the time series and the estimated anoms,
  #' indicated by circles, should also be returned.
  #' @param y_log Apply log scaling to the y-axis. This helps with viewing plots that have extremely
  #' large positive anomalies relative to the rest of the data.
  #' @param xlabel X-axis label to be added to the output plot.
  #' @param ylabel Y-axis label to be added to the output plot.
  
  
  # Check for supported inputs types
  if(!is.data.frame(x)){
    stop("data must be a single data frame.")
  } else {
    if(ncol(x) != 2 || !is.numeric(x[[2]])){
      stop("data must be a 2 column data.frame, with the first column being a set of timestamps, and the second coloumn being numeric values.")
    }
    # Format timestamps if necessary
    if (!(class(x[[1]])[1] == "POSIXlt")) {
      x <- format_timestamp(x)
    }
  }
  # Rename data frame columns if necessary
  if (any((names(x) == c("timestamp", "count")) == FALSE)) {
    colnames(x) <- c("timestamp", "count")
  }
  
  if(!is.logical(na.rm)){
    stop("na.rm must be either TRUE (T) or FALSE (F)")
  }
  
  # Deal with NAs in timestamps
  if(any(is.na(x$timestamp))){
    if(na.rm){
      x <- x[-which(is.na(x$timestamp)), ]
    } else {
      stop("timestamp contains NAs, please set na.rm to TRUE or remove the NAs manually.")
    }
  }
  
  # Sanity check all input parameters
  if(max_anoms > .49){
    stop(paste("max_anoms must be less than 50% of the data points (max_anoms =", round(max_anoms*length(x[[2]]), 0), " data_points =", length(x[[2]]),")."))
  } else if(max_anoms < 0){
    stop("max_anoms must be positive.")
  } else if(max_anoms == 0){
    warning("0 max_anoms results in max_outliers being 0.")
  }
  if(!direction %in% c('pos', 'neg', 'both')){
    stop("direction options are: pos | neg | both.")
  }
  if(!(0.01 <= alpha || alpha <= 0.1)){
    if(verbose) message("Warning: alpha is the statistical signifigance, and is usually between 0.01 and 0.1")
  }
  if(!is.null(only_last) && !only_last %in% c('day','hr')){
    stop("only_last must be either 'day' or 'hr'")
  }
  if(!threshold %in% c('None','med_max','p95','p99')){
    stop("threshold options are: None | med_max | p95 | p99.")
  }
  if(!is.logical(e_value)){
    stop("e_value must be either TRUE (T) or FALSE (F)")
  }
  if(!is.logical(longterm)){
    stop("longterm must be either TRUE (T) or FALSE (F)")
  }
  if(piecewise_median_period_weeks < 2){
    stop("piecewise_median_period_weeks must be at greater than 2 weeks")
  }
  if(!is.logical(plot)){
    stop("plot must be either TRUE (T) or FALSE (F)")
  }
  if(!is.logical(y_log)){
    stop("y_log must be either TRUE (T) or FALSE (F)")
  }
  if(!is.character(xlabel)){
    stop("xlabel must be a string")
  }
  if(!is.character(ylabel)){
    stop("ylabel must be a string")
  }
  if(!is.character(title) && !is.null(title)){
    stop("title must be a string")
  }
  if(is.null(title)){
    title <- ""
  } else {
    title <- paste(title, " : ", sep="")
  }
  
  # -- Main analysis: Perform S-H-ESD
  
  # Derive number of observations in a single day.
  # Although we derive this in S-H-ESD, we also need it to be minutley later on so we do it here first.
  gran <- get_gran(x, 1)
  
  if(gran == "day"){
    num_days_per_line <- 7
    if(is.character(only_last) &&  only_last == 'hr'){
      only_last <- 'day'
    }
  } else {
    num_days_per_line <- 1
  }
  
  # Aggregate data to minutely if secondly
  if(gran == "sec"){
    x <- format_timestamp(aggregate(x[2], format(x[1], "%Y-%m-%d %H:%M:00"), eval(parse(text="sum"))))
  }
  
  period = switch(gran,
                  min = 1440,
                  hr = 24,
                  # if the data is daily, then we need to bump the period to weekly to get multiple examples
                  day = 7)
  num_obs <- length(x[[2]])
  
  if(max_anoms < 1/num_obs){
    max_anoms <- 1/num_obs
  }
  
  # -- Setup for longterm time series
  
  # If longterm is enabled, break the data into subset data frames and store in all_data
  if(longterm){
    # Pre-allocate list with size equal to the number of piecewise_median_period_weeks chunks in x + any left over chunk
    # handle edge cases for daily and single column data period lengths
    if(gran == "day"){
      # STL needs 2*period + 1 observations
      num_obs_in_period <- period*piecewise_median_period_weeks + 1
      num_days_in_period <- (7*piecewise_median_period_weeks) + 1
    } else {
      num_obs_in_period <- period*7*piecewise_median_period_weeks
      num_days_in_period <- (7*piecewise_median_period_weeks)
    }
    
    # Store last date in time series
    last_date <- x[[1]][num_obs]
    
    all_data <- vector(mode="list", length=ceiling(length(x[[1]])/(num_obs_in_period)))
    # Subset x into piecewise_median_period_weeks chunks
    for(j in seq(1,length(x[[1]]), by=num_obs_in_period)){
      start_date <- x[[1]][j]
      end_date <- min(start_date + lubridate::days(num_days_in_period), x[[1]][length(x[[1]])])
      # if there is at least 14 days left, subset it, otherwise subset last_date - 14days
      if(difftime(end_date, start_date, units = "days") == as.difftime(num_days_in_period, units="days")){
        all_data[[ceiling(j/(num_obs_in_period))]] <- subset(x, x[[1]] >= start_date & x[[1]] < end_date)
      }else{
        all_data[[ceiling(j/(num_obs_in_period))]] <- subset(x, x[[1]] > (last_date-lubridate::days(num_days_in_period)) & x[[1]] <= last_date)
      }
    }
  }else{
    # If longterm is not enabled, then just overwrite all_data list with x as the only item
    x$score <- c(length = length(x))
    #all_data <- list(x)
    
    ##########
    # split the data into multi frame
    ##########
    num_obs_in_period <- period
    num_obs_in_an_input <- num_period_in_part * period
    all_data <- vector(mode = "list", length = (ceiling((length(x[[1L]]) - 2 * num_obs_in_an_input) / sample_step)))
    for (j in seq(1, length(x[[1L]]), by = sample_step)) {
      start_date <- x[[1]][j]
      end_date <- min(start_date + lubridate::dminutes(num_obs_in_period * num_period_in_part), x[[1]][length(x[[1]])])
      #end_date <- min(start_date + lubridate::dhours(num_obs_in_period * num_period_in_part), x[[1]][length(x[[1]])])
      all_data[[ceiling(j/(sample_step))]] <- subset(x, x[[1]] >= start_date & x[[1]] < end_date)
      if (end_date == x[[1]][length(x[[1L]])])
        break;
    }
    # end split
    ##################################
  }
  
  # Create empty data frames to store all anoms and seasonal+trend component from decomposition
  all_anoms <- data.frame(timestamp=numeric(0), count=numeric(0), score = numeric(0))
  seasonal_plus_trend <- data.frame(timestamp=numeric(0), count=numeric(0))
  
  # Detect anomalies on all data (either entire data in one-pass, or in 2 week blocks if longterm=TRUE)
  for(i in 1:(length(all_data))) {
    
    anomaly_direction = switch(direction,
                               "pos" = data.frame(one_tail=TRUE, upper_tail=TRUE), # upper-tail only (positive going anomalies)
                               "neg" = data.frame(one_tail=TRUE, upper_tail=FALSE), # lower-tail only (negative going anomalies)
                               "both" = data.frame(one_tail=FALSE, upper_tail=TRUE)) # Both tails. Tail direction is not actually used.
    
    # detect_anoms actually performs the anomaly detection and returns the results in a list containing the anomalies
    # as well as the decomposed components of the time series for further analysis.
    if (verbose) {
      time1 <- Sys.time();
    }
    s_h_esd_timestamps <- detect_anoms(all_data[[i]], k=max_anoms, alpha=alpha, down_sample_step, num_obs_per_period=period, use_decomp=TRUE, use_esd=FALSE,
                                       one_tail=anomaly_direction$one_tail, upper_tail=anomaly_direction$upper_tail, verbose=verbose)
    if (verbose) {
      time2 <- Sys.time();
      calTime <- time2 - time1
      print(calTime[1])  
    }
    
    # store decomposed components in local variable and overwrite s_h_esd_timestamps to contain only the anom timestamps
    anoms_score <- s_h_esd_timestamps$anoms_score
    data_decomp <- s_h_esd_timestamps$stl
    s_h_esd_timestamps <- s_h_esd_timestamps$anoms
    
    # -- Step 3: Use detected anomaly timestamps to extract the actual anomalies (timestamp and value) from the data
    if(!is.null(s_h_esd_timestamps)){
      anoms <- subset(all_data[[i]], (all_data[[i]][[1]] %in% s_h_esd_timestamps))
      for (anoms_index in 1L : length(s_h_esd_timestamps)) {
        anoms$score[which(anoms$timestamp == s_h_esd_timestamps[anoms_index])] <- anoms_score[anoms_index]
      }
    } else {
      anoms <- data.frame(timestamp=numeric(0), count=numeric(0), score = numeric(0))
    }
    
    # Filter the anomalies using one of the thresholding functions if applicable
    if(threshold != "None"){
      # Calculate daily max values
      periodic_maxs <- tapply(x[[2]],as.Date(x[[1]]),FUN=max)
      
      # Calculate the threshold set by the user
      if(threshold == 'med_max'){
        thresh <- median(periodic_maxs)
      }else if (threshold == 'p95'){
        thresh <- quantile(periodic_maxs, .95)
      }else if (threshold == 'p99'){
        thresh <- quantile(periodic_maxs, .99)
      }
      # Remove any anoms below the threshold
      anoms <- subset(anoms, anoms[[2]] >= thresh)
    }
    all_anoms <- rbind(all_anoms, anoms)
    seasonal_plus_trend <- rbind(seasonal_plus_trend, data_decomp)
  }
  
  # Cleanup potential duplicates
  all_anoms <- all_anoms[!duplicated(all_anoms[[1]]), ]
  seasonal_plus_trend <- seasonal_plus_trend[!duplicated(seasonal_plus_trend[[1]]), ]
  
  # use an threshold to filter some low score anomalies
  if (length(all_anoms) > 0) {
    pp <- 1L - alpha/(num_obs_in_an_input)
    tt <- qt(pp, (num_obs_in_an_input - 2L))
    lambda_critical <- tt*(num_obs_in_an_input - 1) / sqrt((num_obs_in_an_input - 2 + tt**2)*num_obs_in_an_input)
    all_anoms <- subset(all_anoms, all_anoms[[3]] >= lambda_critical * anoms_threshold)    
  }

  
  # -- If only_last was set by the user, create subset of the data that represent the most recent day
  if(!is.null(only_last)){
    start_date <- x[[1]][num_obs]-lubridate::days(7)
    start_anoms <- x[[1]][num_obs]-lubridate::days(1)
    if(gran == "day"){
      #TODO: This might be better set up top at the gran check
      breaks <- 3*12
      num_days_per_line <- 7
    } else {
      if(only_last == 'day'){
        breaks <- 12
      }else{
        # We need to change start_date and start_anoms for the hourly only_last option
        start_date <- lubridate::floor_date(x[[1]][num_obs]-lubridate::days(2), "day")
        start_anoms <- x[[1]][num_obs]-lubridate::hours(1)
        breaks <- 3
      }
    }
    
    # subset the last days worth of data
    x_subset_single_day <- subset(x, (x[[1]] > start_anoms))
    # When plotting anoms for the last day only we only show the previous weeks data
    x_subset_week <- subset(x, ((x[[1]] <= start_anoms) & (x[[1]] > start_date)))
    all_anoms <- subset(all_anoms, all_anoms[[1]] >= x_subset_single_day[[1]][1])
    num_obs <- length(x_subset_single_day[[2]])
  }
  
  # Calculate number of anomalies as a percentage
  anom_pct <- (length(all_anoms[[2]]) / num_obs) * 100
  
  # If there are no anoms, then let's exit
  if(anom_pct == 0){
    if(verbose) message("No anomalies detected.")
    return (list("anoms"=data.frame(), "plot"=plot.new()))
  }
  
  if(plot){
    # -- Build title for plots utilizing parameters set by user
    plot_title <-  paste(title, round(anom_pct, digits=2), "% Anomalies (alpha=", alpha, ", direction=", direction,")", sep="")
    if(longterm){
      plot_title <- paste(plot_title, ", longterm=T", sep="")
    }
    
    # -- Plot raw time series data
    color_name <- paste("\"", title, "\"", sep="")
    alpha <- 0.8
    if(!is.null(only_last)){
      xgraph <- ggplot2::ggplot(x_subset_week, ggplot2::aes_string(x="timestamp", y="count")) + ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), text=ggplot2::element_text(size = 14))
      xgraph <- xgraph + ggplot2::geom_line(data=x_subset_week, ggplot2::aes_string(colour=color_name), alpha=alpha*.33) + ggplot2::geom_line(data=x_subset_single_day, ggplot2::aes_string(color=color_name), alpha=alpha)
      week_rng = get_range(x_subset_week, index=2, y_log=y_log)
      day_rng = get_range(x_subset_single_day, index=2, y_log=y_log)
      yrange = c(min(week_rng[1],day_rng[1]), max(week_rng[2],day_rng[2]))
      xgraph <- add_day_labels_datetime(xgraph, breaks=breaks, start=as.POSIXlt(min(x_subset_week[[1]]), tz="UTC"), end=as.POSIXlt(max(x_subset_single_day[[1]]), tz="UTC"), days_per_line=num_days_per_line)
      xgraph <- xgraph + ggplot2::labs(x=xlabel, y=ylabel, title=plot_title)
    }else{
      xgraph <- ggplot2::ggplot(x, ggplot2::aes_string(x="timestamp", y="count")) + ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray60"), panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), text=ggplot2::element_text(size = 14))
      xgraph <- xgraph + ggplot2::geom_line(data=x, ggplot2::aes_string(colour=color_name))
      yrange <- get_range(x, index=2, y_log=y_log)
      xgraph <- xgraph + ggplot2::scale_x_datetime(labels=function(x) ifelse(as.POSIXlt(x, tz="UTC")$hour != 0,strftime(x, format="%kh", tz="UTC"), strftime(x, format="%b %e", tz="UTC")),
                                                   expand=c(0,0))
      xgraph <- xgraph + ggplot2::labs(x=xlabel, y=ylabel, title=plot_title)
    }
    
    # Add anoms to the plot as circles.
    # We add zzz_ to the start of the name to ensure that the anoms are listed after the data sets.
    all_anoms$color <- vector(length = length(all_anoms[[1L]]))
    max_score = max(all_anoms$score)
    min_score = min(all_anoms$score)
    for (index in 1L: length(all_anoms[[1L]])) {
        all_anoms$color[index] = min(round((all_anoms$score[index] - min_score) / (max_score - min_score) * 255) + 1, 255) * 3
    }
    xgraph <- xgraph + ggplot2::geom_point(data=all_anoms, ggplot2::aes_string(color=paste("\"zzz_",title,"\"",sep="")), size = 3,  colour = all_anoms$color)
    
    # Hide legend
    xgraph <- xgraph + ggplot2::theme(legend.position="none")
    
    # Use log scaling if set by user
    xgraph <- xgraph + add_formatted_y(yrange, y_log=y_log)
    
  }
  
  # Fix to make sure date-time is correct and that we retain hms at midnight
  all_anoms[[1]] <- format(all_anoms[[1]], format="%Y-%m-%d %H:%M:%S")
  
  # Store expected values if set by user
  if(e_value) {
    anoms <- data.frame(timestamp=all_anoms[[1]], anoms=all_anoms[[2]], 
                        expected_value=subset(seasonal_plus_trend[[2]], as.POSIXlt(seasonal_plus_trend[[1]], tz="UTC") %in% all_anoms[[1]]),
                        stringsAsFactors=FALSE)
  } else {
    anoms <- data.frame(timestamp=all_anoms[[1]], anoms=all_anoms[[2]], stringsAsFactors=FALSE)
  }
  
  # Make sure we're still a valid POSIXlt datetime.
  # TODO: Make sure we keep original datetime format and timezone.
  anoms$timestamp <- as.POSIXlt(anoms$timestamp, tz="UTC")
  
  # Lastly, return anoms and optionally the plot if requested by the user
  if(plot){
    return (list(anoms = anoms, plot = xgraph))
  } else {
    return (list(anoms = anoms, plot = plot.new()))
  }
}


AnomalyDetectionVec <- function(x, max_anoms=0.10, num_period_in_part = 3, sample_step = 1, anoms_threshold = 1.15, down_sample_step = 10, direction='pos', 
                               alpha=0.05, period=NULL, only_last=F, 
                               threshold='None', e_value=F, longterm_period=NULL, 
                               plot=F, y_log=F, xlabel='', ylabel='count', 
                               title=NULL, verbose=FALSE){
  #' @name AnomalyDetectionVec
  #' @param x Time series as a column data frame, list, or vector, where the column consists of 
  #' the observations.
  #' @param max_anoms Maximum number of anomalies that S-H-ESD will detect as a percentage of the
  #' data.
  #' @param num_period_in_part  the numbers of the period in one input window
  #' @param sample_step   if the input data is too large, we should sample the data with sample_step
  #' @param anoms_threshold  use the threshold to filter the anoms. such as if anoms_threshold = 1.05, 
  #' then we will filter the anoms that exceed the exceptional critical value 100%-105%
  #' @param down_sample_step   the step of down_sample_step to decline the data size to STL decompostion,
  #' each period has a number of 20 is ok. so different data has different down_sample_step 
  #' @param direction Directionality of the anomalies to be detected. Options are: 
  #' \code{'pos' | 'neg' | 'both'}.
  #' @param alpha The level of statistical significance with which to accept or reject anomalies.  
  #' @param period Defines the number of observations in a single period, and used during seasonal 
  #' decomposition.
  #' @param only_last Find and report anomalies only within the last period in the time series.
  #' @param threshold Only report positive going anoms above the threshold specified. Options are: 
  #' \code{'None' | 'med_max' | 'p95' | 'p99'}.
  #' @param e_value Add an additional column to the anoms output containing the expected value.
  #' @param longterm_period Defines the number of observations for which the trend can be considered 
  #' flat. The value should be an integer multiple of the number of observations in a single period. 
  #' This increases anom detection efficacy for time series that are greater than a month.
  #' @param plot A flag indicating if a plot with both the time series and the estimated anoms,
  #' indicated by circles, should also be returned.
  #' @param y_log Apply log scaling to the y-axis. This helps with viewing plots that have extremely
  #' large positive anomalies relative to the rest of the data.
  #' @param xlabel X-axis label to be added to the output plot.
  #' @param ylabel Y-axis label to be added to the output plot.
  
  # Check for supported inputs types and add timestamps
  if(is.data.frame(x) && ncol(x) == 1 && is.numeric(x[[1]])){
    x <- data.frame(timestamp=c(1:length(x[[1]])), count=x[[1]])
  } else if(is.vector(x) || is.list(x) && is.numeric(x)) {
    x <- data.frame(timestamp=c(1:length(x)), count=x)
  } else {
    stop("data must be a single data frame, list, or vector that holds numeric values.")
  }
  
  # Sanity check all input parameterss
  if(max_anoms > .49){
    stop(paste("max_anoms must be less than 50% of the data points (max_anoms =", round(max_anoms*length(x[[2]]), 0), " data_points =", length(x[[2]]),")."))
  }
  if(!direction %in% c('pos', 'neg', 'both')){
    stop("direction options are: pos | neg | both.")
  }
  if(!(0.01 <= alpha || alpha <= 0.1)){
    if(verbose) message("Warning: alpha is the statistical signifigance, and is usually between 0.01 and 0.1")
  }
  if(is.null(period)){
    stop("Period must be set to the number of data points in a single period")
  }
  if(!is.logical(only_last)){
    stop("only_last must be either TRUE (T) or FALSE (F)")
  }
  if(!threshold %in% c('None', 'med_max', 'p95', 'p99')){
    stop("threshold options are: None | med_max | p95 | p99.") 
  }
  if(!is.logical(e_value)){
    stop("e_value must be either TRUE (T) or FALSE (F)")
  }
  if(!is.logical(plot)){
    stop("plot must be either TRUE (T) or FALSE (F)")
  }
  if(!is.logical(y_log)){
    stop("y_log must be either TRUE (T) or FALSE (F)")
  }
  if(!is.character(xlabel)){
    stop("xlabel must be a string")
  }
  if(!is.character(ylabel)){
    stop("ylabel must be a string")
  }
  if(!is.character(title) && !is.null(title)){
    stop("title must be a string")
  }
  if(is.null(title)){
    title <- ""
  } else {
    title <- paste(title, " : ", sep="")
  }
  
  # -- Main analysis: Perform S-H-ESD
  
  num_obs <- length(x[[2]])
  
  if(max_anoms < 1/num_obs){
    max_anoms <- 1/num_obs
  }
  
  # -- Setup for longterm time series
  
  # If longterm is enabled, break the data into subset data frames and store in all_data,
  if(!is.null(longterm_period)){
    all_data <- vector(mode="list", length=ceiling(length(x[[1]])/(longterm_period))) 
    # Subset x into two week chunks
    for(j in seq(1,length(x[[1]]), by=longterm_period)){
      start_index <- x[[1]][j]
      end_index <- min((start_index + longterm_period - 1), num_obs)
      # if there is at least longterm_period left, subset it, otherwise subset last_index - longterm_period
      if((end_index - start_index + 1) == longterm_period){
        all_data[[ceiling(j/(longterm_period))]] <- subset(x, x[[1]] >= start_index & x[[1]] <= end_index)
      }else{
        all_data[[ceiling(j/(longterm_period))]] <- subset(x, x[[1]] > (num_obs-longterm_period) & x[[1]] <= num_obs)
      }
    }
  }else{
    # If longterm is not enabled, then just overwrite all_data list with x as the only item
    x$score <- c(length = length(x))
    #all_data <- list(x)
    ##########
    # split the data into multi frame
    ##########
    num_obs_in_period <- period
    num_obs_in_an_input <- num_period_in_part * period
    #all_data <- vector(mode="list", length = (ceiling(length(x[[1]])/num_obs_in_period) - num_period_in_part + 1))
    all_data <- vector(mode = "list", length = (ceiling((length(x[[1L]]) - 2 * num_obs_in_an_input) / sample_step)))
    for (j in seq(1, length(x[[1L]]), by = sample_step)) {
      start_date <- x[[1]][j]
      end_date <- min(start_date + (num_obs_in_period * num_period_in_part), x[[1]][length(x[[1]])])
      all_data[[ceiling(j/(sample_step))]] <- subset(x, x[[1]] >= start_date & x[[1]] < end_date)
      if (end_date == x[[1]][length(x[[1L]])])
        break;
    }
  }
  
  # Create empty data frames to store all anoms and seasonal+trend component from decomposition
  all_anoms <- data.frame(timestamp=numeric(0), count=numeric(0), score = numeric(0))
  seasonal_plus_trend <- data.frame(timestamp=numeric(0), count=numeric(0))
  
  # Detect anomalies on all data (either entire data in one-pass, or in 2 week blocks if longterm=TRUE)
  for(i in 1:length(all_data)) {
    
    anomaly_direction = switch(direction,
                               "pos" = data.frame(one_tail=TRUE, upper_tail=TRUE), # upper-tail only (positive going anomalies)
                               "neg" = data.frame(one_tail=TRUE, upper_tail=FALSE), # lower-tail only (negative going anomalies)
                               "both" = data.frame(one_tail=FALSE, upper_tail=TRUE)) # Both tails. Tail direction is not actually used.
    
    # detect_anoms actually performs the anomaly detection and returns the results in a list containing the anomalies
    # as well as the decomposed components of the time series for further analysis.
    if (verbose == TRUE) {
      time1 <- Sys.time();
    }
    s_h_esd_timestamps <- detect_anoms(all_data[[i]], k=max_anoms, alpha=alpha, down_sample_step, num_obs_per_period=period, use_decomp=TRUE, use_esd=FALSE, 
                                       one_tail=anomaly_direction$one_tail, upper_tail=anomaly_direction$upper_tail, verbose=verbose) 
    if (verbose == TRUE) {
      time2 <- Sys.time();
      calTime <- time2 - time1
      print(calTime[1])
    }

    # store decomposed components in local variable and overwrite s_h_esd_timestamps to contain only the anom timestamps
    anoms_score <- s_h_esd_timestamps$anoms_score
    data_decomp <- s_h_esd_timestamps$stl
    s_h_esd_timestamps <- s_h_esd_timestamps$anoms
    
    # -- Step 3: Use detected anomaly timestamps to extract the actual anomalies (timestamp and value) from the data
    if(!is.null(s_h_esd_timestamps)){      
      anoms <- subset(all_data[[i]], (all_data[[i]][[1]] %in% s_h_esd_timestamps))
      for (anoms_index in 1L : length(s_h_esd_timestamps)) {
        anoms$score[which(anoms$timestamp == s_h_esd_timestamps[anoms_index])] <- anoms_score[anoms_index]
      }
    } else {
      anoms <- data.frame(timestamp=numeric(0), count=numeric(0))
    }
    
    # Filter the anomalies using one of the thresholding functions if applicable
    if(threshold != "None"){
      # Calculate daily max values
      if(!is.null(longterm_period)){
        periodic_maxs <- tapply(all_data[[i]][[2]], c(0:(longterm_period-1))%/%period, FUN=max)
      }else{
        periodic_maxs <- tapply(all_data[[i]][[2]], c(0:(num_obs-1))%/%period, FUN=max)
      }
      
      # Calculate the threshold set by the user
      if(threshold == 'med_max'){
        thresh <- median(periodic_maxs)
      }else if (threshold == 'p95'){
        thresh <- quantile(periodic_maxs, .95)
      }else if (threshold == 'p99'){
        thresh <- quantile(periodic_maxs, .99)
      }
      # Remove any anoms below the threshold
      anoms <- subset(anoms, anoms[[2]] >= thresh)
    }
    all_anoms <- rbind(all_anoms, anoms)
    seasonal_plus_trend <- rbind(seasonal_plus_trend, data_decomp)
  }
  
  # Cleanup potential duplicates
  all_anoms <- all_anoms[!duplicated(all_anoms[[1]]), ]
  seasonal_plus_trend <- seasonal_plus_trend[!duplicated(seasonal_plus_trend[[1]]), ]
  
  # use an threshold to filter some low score anomalies
  if (length(all_anoms) > 0) {
    pp <- 1L - alpha/(num_obs_in_an_input)
    tt <- qt(pp, (num_obs_in_an_input - 2L))
    lambda_critical <- tt*(num_obs_in_an_input - 1) / sqrt((num_obs_in_an_input - 2 + tt**2)*num_obs_in_an_input)
    all_anoms <- subset(all_anoms, all_anoms[[3]] >= lambda_critical * anoms_threshold)
  }
  
  # -- If only_last was set by the user, create subset of the data that represent the most recent period
  if(only_last){
    x_subset_single_period <- data.frame(timestamp=x[[1]][(num_obs-period+1):num_obs], count=x[[2]][(num_obs-period+1):num_obs])
    # Let's try and show 7 periods prior
    past_obs <- period*7
    # If we don't have that much data, then show what we have - the last period
    if(num_obs < past_obs){
      past_obs <- num_obs-period
    }
    
    # When plotting anoms for the last period only we only show the previous 7 periods of data
    x_subset_previous <- data.frame(timestamp=x[[1]][(num_obs-past_obs+1):(num_obs-period+1)], count=x[[2]][(num_obs-past_obs+1):(num_obs-period+1)])
    
    all_anoms <- subset(all_anoms, all_anoms[[1]] >= x_subset_single_period[[1]][1])
    num_obs <- length(x_subset_single_period[[2]])
  }
  
  # Calculate number of anomalies as a percentage
  anom_pct <- (length(all_anoms[[2]]) / num_obs) * 100
  
  # If there are no anoms, then let's exit
  if(anom_pct == 0){
    if(verbose) message("No anomalies detected.")
    return (list("anoms"=data.frame(), "plot"=plot.new()))
  }
  
  if(plot){
    # -- Build title for plots utilizing parameters set by user
    plot_title <-  paste(title, round(anom_pct, digits=2), "% Anomalies (alpha=", alpha, ", direction=", direction,")", sep="")
    if(!is.null(longterm_period)){
      plot_title <- paste(plot_title, ", longterm=T", sep="")
    }
    
    # -- Plot raw time series data
    color_name <- paste("\"", title, "\"", sep="")
    alpha <- 0.8
    if(only_last){    
      all_data <- rbind(x_subset_previous, x_subset_single_period)
      lines_at <- seq(1, length(all_data[[2]]), period)+min(all_data[[1]])
      xgraph <- ggplot2::ggplot(all_data, ggplot2::aes_string(x="timestamp", y="count")) + ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), text=ggplot2::element_text(size = 14))
      xgraph <- xgraph + ggplot2::geom_line(data=x_subset_previous, ggplot2::aes_string(colour=color_name), alpha=alpha*.33) + ggplot2::geom_line(data=x_subset_single_period, ggplot2::aes_string(color=color_name), alpha=alpha)    
      yrange <- get_range(all_data, index=2, y_log=y_log)
      xgraph <- xgraph + ggplot2::scale_x_continuous(breaks=lines_at, expand=c(0,0))
      xgraph <- xgraph + ggplot2::geom_vline(xintercept=lines_at, color="gray60")
      xgraph <- xgraph + ggplot2::labs(x=xlabel, y=ylabel, title=plot_title)    
    }else{
      num_periods <- num_obs/period
      lines_at <- seq(1, num_obs, period)
      
      # check to see that we don't have too many breaks
      inc <- 2
      while(num_periods > 14){
        num_periods <- num_obs/(period*inc)
        lines_at <- seq(1, num_obs, period*inc)
        inc <- inc + 1
      }
      xgraph <- ggplot2::ggplot(x, ggplot2::aes_string(x="timestamp", y="count")) + ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), text=ggplot2::element_text(size = 14))
      xgraph <- xgraph + ggplot2::geom_line(data=x, ggplot2::aes_string(colour=color_name))
      yrange <- get_range(x, index=2, y_log=y_log)
      xgraph <- xgraph + ggplot2::scale_x_continuous(breaks=lines_at, expand=c(0,0))
      xgraph <- xgraph + ggplot2::geom_vline(xintercept=lines_at, color="gray60")
      xgraph <- xgraph + ggplot2::labs(x=xlabel, y=ylabel, title=plot_title)
    }
    
    # Add anoms to the plot as circles.
    # We add zzz_ to the start of the name to ensure that the anoms are listed after the data sets.
    all_anoms$color <- vector(length = length(all_anoms[[1L]]))
    max_score = max(all_anoms$score)
    min_score = min(all_anoms$score)
    for (index in 1L: length(all_anoms[[1L]])) {
      all_anoms$color[index] = min(round((all_anoms$score[index] - min_score) / (max_score - min_score) * 255) + 1, 255)
    }
    xgraph <- xgraph + ggplot2::geom_point(data=all_anoms, ggplot2::aes_string(color=paste("\"zzz_",title,"\"",sep="")), size = 3, colour = all_anoms$color)
    
    # Hide legend and timestamps
    xgraph <- xgraph + ggplot2::theme(axis.text.x=ggplot2::element_blank()) + ggplot2::theme(legend.position="none") 
    
    # Use log scaling if set by user
    xgraph <- xgraph + add_formatted_y(yrange, y_log=y_log)
  }
  
  # Store expected values if set by user
  if(e_value) {
    anoms <- data.frame(index=all_anoms[[1]], anoms=all_anoms[[2]], expected_value=subset(seasonal_plus_trend[[2]], seasonal_plus_trend[[1]] %in% all_anoms[[1]]))  
  } else {
    anoms <- data.frame(index=all_anoms[[1]], anoms=all_anoms[[2]])
  }
  
  # Lastly, return anoms and optionally the plot if requested by the user
  if(plot){
    return (list(anoms = anoms, plot = xgraph))
  } else {
    return (list(anoms = anoms, plot = plot.new()))
  }
}
