Parameter $d$ is associated with the *timing* weight of the episode. This value can be adjusted based on the extent of the episode's overlap with a special period. Timing weight is monotone increasing with $d$. The functional form for the timing weight is:
    $$ w_3(n_k,m) = 1 - exp\\left(-d\\left(\\frac{\\tau_k}{n_k}\\right)\\right)$$
