Source data should be stored as a `CSV` format with two columns and header.

* The first column is for the time variable (could be year, month, day, etc.). It should be in the format YYYY-MM-DD.
* The second column is for the IMPIT index value. 
* Please note that missing values are not allowed and space in header's names is not allowed either (e.g., use 'impit_index' instead of 'impit index').

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
  \\text{Time} & \\text{Index} \\\\ 
  \\hline 
  \\text{1988-01-01} & 1.49 \\\\ 
  \\hline 
  \\text{1989-01-01} & 1.51 \\\\
  \\hline 
  \\text{1990-01-01} & 1.50 \\\\
  \\hline 
  \\vdots & \\vdots \\\\ 
  \\hline 
  \\text{2019-01-01} & 0.91 \\\\ 
  \\hline 
\\end{array}
$$
