gather_timfoc_dates <- function(start_day, start_month, end_day, end_month){
  
  choices_days <- as.character(1:31)
  choices_months <- c("January","February","March","April","May",
                      "June","July","August","September","October",
                      "November","December") 

  # convert days from numeric to text  
  day1 <- ifelse(start_day %in% seq(1,9,1), paste0("0",start_day), paste0(start_day))
  day2 <- ifelse(end_day %in% seq(1,9,1), paste0("0",end_day), paste0(end_day))
  
  # convert months from numeric to text
  start_month <- which(choices_months == start_month)
  month1 <- ifelse(start_month %in% seq(1,9,1), paste0("0",start_month), paste0(start_month))
  end_month <- which(choices_months == end_month)
  month2 <- ifelse(end_month %in% seq(1,9,1), paste0("0",end_month), paste0(end_month))
  
  # gather as a vector
  period_timfoc <- c(paste0(month1,"-",day1), paste0(month2,"-",day2))
  
  return(period_timfoc)
  
}
  