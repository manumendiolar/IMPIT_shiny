# Function to compute the recency of an episode 
# associated with its relative importance weight

# Input
# b  : numeric value representing the dampening of the exponential form
#
# nu : numeric value  or vector from fun_nu2() 

# Output
# w2 : numeric value 


fun_w2 <- function(b, nu2){
  
  # make sure nu2 falls \in [0,1]
  d <- max(nu2)
  nu2 <- ifelse( nu2 > 0, nu2 / d, nu2)
  # compute weight
  w2 <- exp(-b*(1-nu2))
  return(w2)
}