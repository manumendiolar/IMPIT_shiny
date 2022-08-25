
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
# index_range: vector of dates when we want to compute IMPIT index.
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



fun_IMPITv2 <- function(episodes, unit, index_range, m, a, b, c, d=NULL, intensity, time_focus=FALSE, tau=NULL){

  # Pre-allocate IMPIT index vector
  Index <- numeric(length(index_range))
  
  # In each step tt we compute index (step tt corresponds to date index_range[tt])
  for (tt in 1:length(index_range)){
    
    # Step tt
    date_now <- index_range[tt]
    date_bef <- subtract_mem(date_now, m , unit)
    
    # Period to analyze episodes
    period <- seq(date_bef, date_now, by = unit) 
    
    # Keep start and end (in case we need to update them)
    period_start <- head(period, 1)
    period_end <- tail(period, 1) 
    
    # Identify episodes in that period
    sub <- subset(episodes, date_start %in% period | date_end %in% period)
    
    # pre-allocate
    nu2 <- NULL
    nu3 <- NULL
    w1 <- NULL
    w2 <- NULL
    w3 <- NULL
    wEk <- NULL
    IEk <- NULL
    
    # If we have episodes let's compute their intensity and weights
    if (dim(sub)[1]>0){
      
      # Check if first episode started before the start of the period
      # if it does update period_start
      if ( as.Date(min(sub$date_start)) < period_start ){
        period_start <- as.Date(min(sub$date_start))
      }
      
      # Check if last episode ended after the end of the period
      # if it does update period_end
      if ( as.Date(max(sub$date_end)) > period_end ){
        period_end <-as.Date(max(sub$date_end))
      }
      
      # Update whole period array
      period <- seq(period_start, period_end, by=unit) 
      
      # M should be approx m (always in the same units as the original data)
      M <- length(period) 
      
      # For each episode 
      for (kk in 1:dim(sub)[1]){
        
        # Compute starting position
        sk <- M - which(period == sub$date_start[kk]) + 1
        
        # Save duration
        nk <- sub$duration[kk]
        
        # Compute duration of overlapping with special season
        # as of now timing weight only works for monthly or daily data
        if (time_focus) { 
          if (unit == "months") {
            tk <- sub$overlap_months[kk] 
          } else {
            tk <- sub$overlap_days[kk]
          }
        }
        
        # Keep intensity 
        IEk <- c(IEk, as.numeric(sub[kk, paste0("intensity_",intensity)]))
        
        # Compute persistence weight
        w1 <- c(w1, fun_w1(a=a, M=M, nk=nk))
        
        # Pre-compute recency weight
        nu2 <- c(nu2, fun_nu2(c=c, M=M, sk=sk))
        
        # Pre-compute timing weight 
        if (time_focus) { nu3 <- c(nu3, fun_nu3(t=tau,tk=tk,nk=nk)) }

      } # end of analysis in step tt 
      
      # Compute final recency weight
      w2 <- fun_w2(b=b, nu2=nu2)
      
      # Compute final timing weight 
      if (time_focus) { 
        w3 <- fun_w3(d=d, nu3=nu3)
      } else {
        w3 <- rep(1, dim(sub)[1])
      }
      
      # Compute relative importance weight (contribution of w1, w2 and w3)
      wEk <- w1*w2*w3
      
    } else {
      # If we don't have episodes in step tt
      IEk <- 0
      wEk <- 0
    }
    
    # Compute index value in step tt and save 
    Index[tt] <- sum(wEk*IEk)
    
  } # repeat for next step ...
  
  return(Index)
}

