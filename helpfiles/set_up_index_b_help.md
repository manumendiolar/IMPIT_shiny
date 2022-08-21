Associated with the *recency* of the episode. Parameter $b$ captures the rate of decay of recency weight as the starting date deviates more from its peak. Low values flatten the relative importance weight. The functional form for the recency weight is:
    $$ w_2(s_k,m) = exp\\left[ -b \\left( 1- \\lambda \\right(\\frac{s_k}{m} \\left)^c \\right(1-\\frac{s_k}{m} \\left)^{1-c} \\right) \\right]$$

where $s_k$ is the length of an episode $E_k$, $m$ is the memory and $\\lambda > 0$ is chosen to ensure that 
     $$ \\right(\\frac{s_k}{m} \\left)^c \\right(1-\\frac{s_k}{m} \\left)^{1-c} \\right) \\right]$$
     