subtract_mem <- function(date_now, m, unit){
  
  # this function computes the exact date (date_before) 
  # m unites before current date (date_now)
  
  if (unit == "days"){
    date_before <- date_now - lubridate::days(m)
  } else {
    if (unit == "months"){
      date_before <- date_now %m-% months(m) 
    } else {
      date_before <- date_now - lubridate::years(m)
    }
  }
  return(date_before)
}