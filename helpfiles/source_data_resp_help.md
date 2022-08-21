Source data for the response variable should be stored as a `CSV` format with two columns and header.

* The first column is for the time variable in years (numeric value). 
* The second column is for the response value (numeric value). 
* Please note that missing values are not allowed and space in header's names is not allowed either (e.g., use 'resp_variable' instead of 'resp variable').

***
CSV Format:

$$
\\begin{array}{rr} 
  \\hline 
  \\text{Time} & \\text{Value} \\\\ 
  \\hline 
  \\cdot & \\cdot \\\\ 
  \\cdot & \\cdot \\\\ 
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
  \\text{1988} & 52.85 \\\\ 
  \\text{1989} & 61.20 \\\\
  \\text{1990} & 49.42 \\\\
  \\vdots & \\vdots \\\\ 
  \\text{2019} & 15.94 \\\\ 
  \\hline 
\\end{array}
$$