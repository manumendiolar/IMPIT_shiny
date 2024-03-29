# Function to compute information about timing focus 

# Input
#
# episodes : output of mydetect_event(). A data frame of episodes 
#            corresponding to SOI values above/below threshold and
#            with a minimum duration "block length".
#
# timfoc_dates : character vector of length 2. Corresponding to start and date of 
#                special season. Format "MM-DD". 


# Output
#
# episodes : episode input with 5 extra columns 
#            overlap                 : logical value indicating if there was overlapping (TRUE) or not (FALSE)
#            overlap_days            : integer value with duration of the overlap in days 
#            overlap__months         : integer value with duration of the overlap in months
#            overlap_date_start      : character with format "YYYY-MM-DD" for the date when the overlap starts
#            overlap_date_end        : character with format "YYYY-MM-DD" for the date when the overlap ends


# Note
# Depends on user defined functions fun_getdates() and fun_overlap()
# Depends on library lubridate 


mydetect_timfoc <- function(episodes, timfoc_dates){
  
  if (episodes[1,1] !=0) {
  # arrange format
  episodes$date_start <- as.Date(episodes$date_start, format = "%Y-%m-%d")
  episodes$date_peak <- as.Date(episodes$date_peak, format = "%Y-%m-%d")
  episodes$date_end <- as.Date(episodes$date_end, format = "%Y-%m-%d")
  
  # pre-allocate
  episodes$overlap <- FALSE
  episodes$overlap_days <- 0
  episodes$overlap_months <- 0

  
    
    for (ii in 1:dim(episodes)[1]){
      
      # extract info dates from episodes
      Edates <- c(episodes$date_start[ii], episodes$date_end[ii])
      
      # Note: date format should already be YYYY-MM-DD
    
      # if episode covered more than a year (we analyze overlap in each year)
      dates <- fun_getdates(Edates)
  
      overlap <- FALSE
      overlap_duration_days <- 0
      overlap_duration_months <- 0
      dates_overlap <- NULL
    
      for (jj in 1:length(dates)){
        
        # add year info to Tdates
        Tdates <- as.Date(paste(lubridate::year(dates[[jj]]),timfoc_dates,sep="-"), format = "%Y-%m-%d") 
        
        # get overlap info
        out <- fun_overlap(dates[[jj]], Tdates)
        
        if (out$overlap){
          
          # sufficient if entering once
          overlap <- TRUE
          
          # number of days of overlap
          overlap_duration_days <- overlap_duration_days + (as.Date(out$date_end_overlap) - as.Date(out$date_start_overlap) + 1)
        
          # number of months of overlap
          overlap_duration_months <- overlap_duration_months + length(seq(lubridate::month(out$date_start_overlap),
                                                                          lubridate::month(out$date_end_overlap),
                                                                          1))
          # save dates when overlap
          #dates_overlap <- rbind(dates_overlap, c(out$date_start_overlap, out$date_end_overlap) ) 
        }
      }
      
      # add overlap info to episodes data frame
      if (overlap) {
        episodes$overlap[ii] <- overlap
        episodes$overlap_days[ii] <- overlap_duration_days
        episodes$overlap_months[ii] <- overlap_duration_months
      }
    }
  }
  
  return(episodes)
}


