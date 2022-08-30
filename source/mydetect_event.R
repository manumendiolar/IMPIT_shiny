# Function to compute episodes from time series of an environmental signal 

# INUPUT
# dat         : data frame of environmental signal. This has 4 columns: 
#               year (integer), month (integer), day (integer) and value (numeric) of the signal. 
#
# thres_above : to indicate if episodes should be constructed for values above or below certain threshold. thres_abov should be a logical value (TRUE/FALSE).
#
# thres       : value of threshold (numeric) 
# 
# duration_min: minimum duration of consecutive values above/below threshold. 

# OUTPUT
# data frame of episodes  

# Note
# Depends on user defined functions fun_nu() and fun_w()
# Depends on libraries tidyverse, lubridate 


mydetect_event <- function(dat, thres_above, thres, duration_min){
  
  # dat: data frame
  # thres_above: logical
  # thres: choose threshold
  # duration_min: integer
  
  # prepare data
  #names(dat) <- c("date","EnvSignal")
  #dat <- dat %>%  mutate(date = as.Date(date,"%Y-%m-%d")) # this should be already the format.
  names(dat) <- c("year","month","day","EnvSignal")
  dat <- dat %>%  mutate(date = paste0(year,"-",month,"-",day))
  dat <- dat %>%  mutate(date = as.Date(date,"%Y-%m-%d")) 
  dat <- dat[order(dat$date), ] # order rows according to date
  ind <- NULL
  
  
  # identify episodes (detect "blocks" of values exceeding thresh)
  if (thres_above) {
    aa <- which(dat$EnvSignal >= thres)
  } else {
    aa <- which(dat$EnvSignal < thres)
  }
  
  if (length(aa) > 0) {
    ind <- split(aa, cumsum(c(1, diff(aa) != 1)))
    
    # if also interested on block with a minimum length
    if (!is.null(duration_min)) {
      bb <- sapply(ind, function(x) length(x) >= duration_min)
      if (any(bb)) {
        ind <- ind[bb]
      } else {
        ind <- NULL
      }
    }
    
    if (length(ind) != 0){
      
      # extract info from each episode (number of columns could vary ?)
      no_events <- length(ind)
      
      # pre-allocate
      episodes <- data.frame(event_no            = integer(no_events),
                             index_start         = integer(no_events),
                             index_peak          = integer(no_events),
                             index_end           = integer(no_events),
                             duration            = integer(no_events),
                             date_start          = character(no_events),
                             date_peak           = character(no_events),
                             date_end            = character(no_events),
                             intensity_mean      = numeric(no_events),
                             intensity_median    = numeric(no_events),
                             intensity_max       = numeric(no_events),
                             intensity_min       = numeric(no_events),
                             intensity_log       = numeric(no_events)
                             )
      
      for (ii in 1:length(ind)){
        
        # number of event
        event_no <- ii
       
        # keep index of row
        index_start <- head(ind[[ii]],1)
        index_peak <- ind[[ii]][which(dat[ind[[ii]],2] == max(dat[ind[[ii]],2]))][1] #just keep first one in case values repeated
        index_end <- tail(ind[[ii]],1)
        
        # length of the event
        duration <- length(ind[[ii]])
        
        # keep dates (according to row index)
        date_start <- dat[index_start, ]$date
        date_peak <- dat[index_peak, ]$date
        date_end <- dat[index_end, ]$date
       
        # a couple of intensities (we could also put a function here) 
        intensity_mean   <- mean(t(dat[ind[[ii]],"EnvSignal"]))
        intensity_median <- median(t(dat[ind[[ii]],"EnvSignal"]))
        intensity_max    <- max(t(dat[ind[[ii]],"EnvSignal"]))
        intensity_min    <- min(t(dat[ind[[ii]],"EnvSignal"]))
        intensity_log    <- ifelse(sum(dat[ind[[ii]],"EnvSignal"]) > 0, log(sum(dat[ind[[ii]],"EnvSignal"])), -log(abs(sum(dat[ind[[ii]],"EnvSignal"]))))
        
        # save info
        episodes$event_no[ii] <- ii
        episodes$index_start[ii] <- index_start
        episodes$index_peak[ii] <- index_peak
        episodes$index_end[ii] <- index_end
        episodes$duration[ii] <- duration
        episodes$date_start[ii] <- as.character(date_start)
        episodes$date_peak[ii] <- as.character(date_peak)
        episodes$date_end[ii] <- as.character(date_end)
        episodes$intensity_mean[ii] <- intensity_mean
        episodes$intensity_median[ii] <- intensity_median
        episodes$intensity_max[ii] <- intensity_max
        episodes$intensity_min[ii] <- intensity_min
        episodes$intensity_log[ii] <- intensity_log
      }
      
      # columns we want
      col_names <- c("event_no","duration", "date_start","date_peak","date_end",
                    "intensity_mean","intensity_median","intensity_max","intensity_min","intensity_log")
      # keep only the columns specified above 
      episodes <- episodes[ ,col_names]
  
    } else {
      # empty data frame
      episodes <- data.frame(event_no = integer(1), duration = 0, date_start = 0, date_peak = 0, date_end = 0,
                             intensity_mean = 0, intensity_median = 0, intensity_max = 0, intensity_min = 0, intensity_log = 0)
    }
  } else {
    
    # empty data frame
    episodes <- data.frame( 
      event_no = integer(1), 
      duration = integer(1),
      date_start = integer(1),
      date_peak = integer(1),
      date_end = integer(1),
      intensity_mean = numeric(1),
      intensity_median = numeric(1),
      intensity_max = numeric(1),
      intensity_min = numeric(1),
      intensity_log = numeric(1)
    )
    #episodes <- episodes[NULL, ]
  }
  
  return(episodes)
  
}

