# Function to check if there is overlap between episode and special season

# Input
# Edates : vector of dates for episode E. First element is the start date of the episode and second element the end date.
#
# Tdates : vector of dates for special season Tau. First and second element are its start and end date, respectively. 

# Output
# a list with three elements
#   overlap            : a logic value TRUE if there is overlap, FALSE otherwise.  
#   date_start_overlap : date with format "MM-DD" when the overlap starts
#   date_end_overlap   : date with format "MM-DD" when the overlap end

# Note: both pair of dates should be in the same year (for now)



fun_overlap <- function(Edates, Tdates){
  
  date_start_overlap <- max(Edates[1], Tdates[1])
  date_end_overlap <- min(Edates[2], Tdates[2])
  
  overlap <- date_start_overlap <= date_end_overlap
  
  date_start_overlap <- ifelse(overlap, date_start_overlap, NA)
  date_end_overlap <- ifelse(overlap, date_end_overlap, NA)
  
  out <- list(overlap = overlap, 
              date_start_overlap = date_start_overlap,
              date_end_overlap = date_end_overlap)
  
  return(out)
  
}