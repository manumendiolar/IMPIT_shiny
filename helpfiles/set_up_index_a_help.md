Associated with the *persistence* of the episode. Parameter $a$ dampens the rate of decay. A value close to zero means that each episode will have nearly the same importance weight regardless of its duration. The functional form for the persistence weight is:
  $$ w_1(n_k,m) = exp\\left(-a\\left(1-\\frac{n_k}{m}\\right)\\right)$$

where $n_k$ is the length of an episode $E_k$ and $m$ is the memory.  