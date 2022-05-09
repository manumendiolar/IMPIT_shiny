# Function to break a period within each year of the period

# Input
# Edates : vector of dates for episode E. First element is the start date of the episode and second element the end date.


# Output
# a list of vectors with dates information. The length of the list will depend on the years covered by the episode.



fun_getdates <- function(Edates){
  
  # Note: format od dates should be YYYY-MM-DD
  
  # vector of years covered by the episode
  years <- seq(lubridate::year(Edates[1]), lubridate::year(Edates[2]), 1)
  
  # pre-allocate
  dates <- vector(mode = "list", length = length(years))
  
  
  if (length(years) > 1){
    
    for (ii in 1:length(years)){
     
      begin <- paste0(years[[ii]],"-01-01" )
     
      end <- paste0(years[ii],"-12-31")
     
      dates[[ii]] <- c(begin, end)
    }
    
    # update begin
    begin <- max(dates[[1]][1], Edates[1])
    dates[[1]][1] <- begin
    
    # update end
    end <- min(dates[[length(years)]][2], Edates[2])
    dates[[length(years)]][2] <- end
  
  } else {
    
    dates[[1]] <- c(Edates[1], Edates[2])
    
  }
  
  return(dates)
  
}