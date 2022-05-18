
# Function to compute IMPIT index for an environmental signal 


# INPUT
# *****
#
# episodes   : data frame. Episodes associated to the environmental signal
#              under study. User can generate this using mydetect_event().
#              The format of the data frame should be the same at mydetect_event(). 
#
# unit       : string value. To indicate if episodes are in 'days' or 'months'.
#
# yrs        : integer vector. Years we want to compute IMPIT index.
#
# m          : integer value. Memory, in years.
#
# a          : numeric value. Dampening parameter of the persistence weight w1.
# 
# b          : numeric value. Dampening parameter of the recency weight w2.
#
# c          : numeric value. Parameter \in [0,1] for the recency weight w2.
#
# d          : numeric value. Dampening parameter of timing focus weight w3. 
#
# intensity  : string value. Indicating the type of intensity to be considered. Options:
#              mean, min, max, median, log. 
#
# time_focus : logical value. TRUE/FALSE indicating if timing focus is taken into account.
#
# tau        : numeric value. Length of timing focus period/ season. NULL if time_focus = FALSE.
#




# OUTPUT
# ******
#
# Index      : numeric vector. Indices for each year indicated by yrs input. 



# NOTE
# Dependencies functions: fun_nu2(), fun_nu3(), fun_w1(), fun_w2() and fun_w3(). 
# Dependencies libraries: lubridate.


fun_IMPIT <- function(episodes, unit, yrs, m, a, b, c, d, intensity, time_focus=FALSE, tau=NULL){
  
  # vector to place index
  Index <- numeric(length(yrs))
  
  
  # converting memory from years to months
  M <- m*12
  
  # add year column for each episode 
  episodes$year_start <- lubridate::year(episodes$date_start)
  episodes$year_end <- lubridate::year(episodes$date_end)
  
  # compute index in each year
  for (tt in 1:length(yrs)){
    
    # array of years to take into account (now yrs[tt] and m years before)
    period <- seq(yrs[tt]-(m-1), yrs[tt], 1) 
    
    # keep start and end date of the period
    date_start <- as.Date(paste0(head(period,1),"-01-01"))
    date_end <- as.Date(paste0(tail(period,1),"-12-31"))
    
    # vector of dates of the period
    dates <- seq(date_start, date_end, by=unit) 
    
    # identify episodes in that period
    sub <- subset(episodes, year_start %in% period | year_end %in% period)
   
    # pre-allocate
    nu2 <- NULL
    nu3 <- NULL
    w1 <- NULL
    w2 <- NULL
    w3 <- NULL
    wEk <- NULL
    IEk <- NULL
    
    
    # if we have episodes
    if (dim(sub)[1]>0){
      
      # We might need to update the vector of dates
      # perhaps one event started in previous date but ended in the period
      # or started at the end of the period and finished outside
      
      # check if begun before beginning of the period
      if ( as.Date(min(sub$date_start)) < date_start ){
        # update
        date_start <- as.Date(min(sub$date_start))
        #dates <- c(seq(as.Date(min(sub$date_start)), date_start, by="months"), dates[-1])
       
      }
      # check if ended after the ending of the period
      if ( as.Date(max(sub$date_end)) >  date_end ){
        # update
        date_end <-as.Date(max(sub$date_end))
        #dates <- c(dates[-length(dates)], seq(date_end, as.Date(max(sub$date_end)), by="months"))
      }
      
      # update vector of dates of the period
      dates <- seq(date_start, date_end, by=unit) 
      
      # update M
      M <- length(dates)
      
      # analysis for each episode
      for (kk in 1:dim(sub)[1]){
       
        # starting position
        sk <- M - which(dates == sub$date_start[kk]) + 1
       
        # duration
        nk <- sub$duration[kk]
        
        # duration of overlapping with special season if time_focus
        if (time_focus) { 
          if (unit == "months") {
            tk <- sub$overlap_months[kk] 
          } else {
            tk <- sub$overlap_days[kk]
          }
        }
          
        
        # keep intensity 
        IEk <- c(IEk, as.numeric(sub[kk, paste0("intensity_",intensity)]))
        
        # persistence weight w1
        w1 <- c(w1, fun_w1(a=a, M=M, nk=nk))
       
        # pre-computation for w2
        nu2 <- c(nu2, fun_nu2(c=c, M=M, sk=sk))
       
        # pre-computation for w3
        if (time_focus) { nu3 <- c(nu3, fun_nu3(t=tau,tk=tk,nk=nk)) }
       
      }
      
      # recency weight w2
      w2 <- fun_w2(b=b, nu2=nu2)
      
      # timing focus weight w3 
      if (time_focus) { 
        w3 <- fun_w3(d=d, nu3=nu3)
      } else {
        w3 <- rep(1, dim(sub)[1])
      }
      
      
      # compute relative importance weight
      wEk <- w1*w2*w3
       
    } else {
      IEk <- 0
      wEk <- 0
    }
    
    # compute index and save 
    Index[tt] <- sum(wEk*IEk)
    
    # repeat for next year...
  }
  
  return(Index)
}

