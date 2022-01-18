# Auxiliary function for w2 weight 

# Input
# c  : numeric value \in [0,1]. A low value assigns more importance to recent episodes
#
# M  : integer value representing memory in years
#
# sk : integer value representing the starting position of episode k

# Output
# nu2 : numeric value 


fun_nu2 <- function(c,M,sk){
  nu2 <- ((sk/M)^c)*((1-(sk/M))^(1-c))
  return(nu2)
}
