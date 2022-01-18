# Function to compute relative importance weight of an episode 
# when empirical data from "response" variable is available.

# Input
# d  : numeric value to represent the dampening of the exponential form
#
# nu : numeric value or vector of ratio nu representing the contribution of all age groups younger than a given age.

# Output
# w  : numeric value or vector of relative importance weight


fun_w <- function(d,nu){
  w <- exp(-d*(1-nu))
  return(w)
}