# Auxiliary function for w3 weight (timing focus) 

# Input
# t  : integer value representing the length of the special season
#
# tk : integer value representing the length of episode Ek that falls in the special season
#
# nk : integer value representing the length of episode Ek
#


# Output
# nu3 : numeric value 


fun_nu3 <- function(t,tk,nk){
  
  nu3 <- (tk/nk)
  #nu3 <- (tk/t)
  #nu3 <- (tk/nk)*(tk/t)
  
  return(nu3)
}
