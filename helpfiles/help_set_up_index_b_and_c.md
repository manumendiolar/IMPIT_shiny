Associated with the *recency* of the episode. Parameter $b$ captures the rate of decay of recency weight as the starting date deviates more from its peak. Low values flatten the relative importance weight. Parameter $c$ characterizes the skewness of the recency weight and its peak. A value close to zero indicates that recent episodes have higher weight than those starting late in the memory. 

The functional form for the recency weight is defined by:
    $$ \\nu(s_k,m)  = \\lambda \\left(\\frac{s_k}{m} \\right)^c \\left(1-\\frac{s_k}{m}\\right)^{1-c}$$

where $s_k$ is the starting time of episode $E_k$, $m$ is the memory, $c \\in [0,1]$ and $\\lambda > 0$ is chosen to ensure that  $\\nu(s_k,m) \\leq 1$. To capture the relative importance of recency of episode $E_k$ within the period considered:

   $$ w_2(s_k,m) = exp\\left[ -b \\left( 1- \\nu(s_k,m) \\right) \\right]$$

where $b > 0$. 
     
     