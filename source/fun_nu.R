# Function to generate ratios nu 
# they represent the contribution of all age groups younger than a given age.
# we need this function to compute relative importance weights w when empirical data is available

# Input
# freq : numeric vector of proportions for each age group starting with age 0
#
# agej : integer denoting the age limit we should look to compute contribution

# Output
# nu : numeric value 


fun_nu <- function(freq, agej){
  agej <- agej + 1 
  if (agej > length(freq)) agej <- length(freq)
  sumj <- sum(freq[agej:length(freq)]) 
  sumT <- sum(freq)
  nu <- sumj / sumT
  return(nu)
}
