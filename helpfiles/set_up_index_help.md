To start building IMPIT index these are the guidelines: 

* First, enter a value for the *memory* ($m$) in years. A period of fixed and uninterrupted duration in the past with respect to the current observation in time. Note that according to $m$ the app automatically sets the first year of resulting IMPIT index. 
* Second, choose an intensity function. If you generate the episode list you can choose between: mean, median, max (maximum), min (minimum) or log (natural logarithm). But if you uploaded the episode list this menu will depend entirely on the intensity columns provided in that dataset. 
* Thirdly, choose the parameters for each of the relative importance weights: 
    * $a$: enter a value for the parameter associated to the *persistence* of the episode. This parameter dampens the rate of decay. A value close to zero means that each episode will have nearly the same importance weight regardless of its duration. The functional form for this weight is:
    $$ w_1(n_k,m) = exp\\left(-a\\left(1-\\frac{n_k}{m}\\right)\\right)$$.
    * $b$ 
    * $c$: enter a value for the parameters associated to the *recency* of the episode. The functional form for this weight is:
    $$ w_2(s_k,m) = exp\\left[ -b \\left( 1- \\lambda \\right(\\frac{s_k}{m} \\left)^c \\right(1-\\frac{s_k}{m} \\left)^{1-c} \\right) \\right]$$
    * $d$: enter a value for the parameter associated to the *persistence* of the event. The functional form for this weight is:
    $$ w_3(n_k,m) = 1 - exp\\left(-d\\left(\\frac{\\tau_k}{n_k}\\right)\\right)$$


* Please note that missing values are not allowed and space in header's names is not allowed either (e.g., use 'soi_episodes' instead of 'soi episodes').


*** 
An example:


