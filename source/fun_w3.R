# Function to compute the timing focus of an episode 
# associated with its relative importance weight

# Input
# b  : numeric value representing the dampening of the exponential form
#
# nu : numeric value  or vector from fun_nu2() 

# Output
# w2 : numeric value 


fun_w3 <- function(d, nu3){
  
  # make sure nu falls \in [0,1]
  aux <- max(nu3)
  nu3 <- ifelse( nu3 > 0, nu3 / aux, nu3)
  
  # compute weight
  w3 <- 1 - exp(-d*nu3)
  return(w3)
}

