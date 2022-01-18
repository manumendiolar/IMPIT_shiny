# Function to compute the persistence of an episode 
# associated with its relative importance weight

# Input
# a  : numeric value representing the dampening of the exponential form
#
# M  : integer value representing memory in years
# 
# nk : integer value representing the length of the episode (in months)

# Output
# w1 : numeric value 


fun_w1 <- function(a,M,nk){
  w1 <- exp(a*((nk-M)/M))
  return(w1)
}