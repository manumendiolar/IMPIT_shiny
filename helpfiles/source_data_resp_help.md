Source data should be stored as a `CSV` format with two columns and header.

* The first column is for the time variable (could be year, month, day, etc.). It should be in the format YYYY-MM-DD.
* The second column is for the response variable value. 
* Please note that missing values are not allowed and space in header's names is not allowed either (e.g., use 'resp_variable' instead of 'response variable').

***
CSV Format:

$$
\\scriptsize
\\begin{array}{cc} 
  \\hline 
  \\text{Time} & \\text{Value} \\\\
  \\hline 
  \\cdot & \\cdot \\\\ 
  \\hline 
\\end{array}
$$


*** 
An example:

$$
\\begin{array}{rr} 
  \\hline 
  \\text{Year} & \\text{catch_rate} \\\\ 
  \\hline 
  \\text{1988-01-01} & 52.85 \\\\ 
  \\hline 
  \\text{1989-01-01} & 61.20 \\\\
  \\hline 
  \\text{1990-01-01} & 49.42 \\\\
  \\hline 
  \\vdots & \\vdots \\\\ 
  \\hline 
  \\text{2019-01-01} & 15.94 \\\\ 
  \\hline 
\\end{array}
$$