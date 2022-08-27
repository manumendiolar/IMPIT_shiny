mydetect_timeunits <- function(episodes){
  
  # This function computes the time units of an episode list
  episodes_units <- "warning"
  
  # check if we have elements
  if (episodes[1,1] != 0) {
    
    irow  <- sample.int(n = dim(episodes)[1], size = 1, replace = FALSE)
    
    istart <- episodes[irow, "date_start"]
    iend <- episodes[irow, "date_end"]
    
    istart_dd <- lubridate::day(istart)
    istart_mm <- lubridate::month(istart)
    istart_yy <- lubridate::year(istart)
    
    iend_dd <- lubridate::day(iend)
    iend_mm <- lubridate::month(iend)
    iend_yy <- lubridate::year(iend)
    
    if ( istart_dd != iend_dd )  episodes_units <- "days"
    if ( (istart_dd == iend_dd) & (istart_mm != iend_mm) ) episodes_units <- "months"
    if ( (istart_dd == iend_dd) & (istart_mm == iend_mm) & (istart_yy == iend_yy) ) episodes_units <- "years"
  }
  
  return(episodes_units)
}

