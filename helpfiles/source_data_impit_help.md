Source IMPIT data should be stored as a `CSV` format with two columns and header.

* The first column is for the time variable in years (numeric value). 
* The second column is for the IMPIT index value (numeric value). 
* Please note that missing values are not allowed and space in header's names is not allowed either (e.g., use 'impit_index' instead of 'impit index').

***
CSV Format:

$$
\\begin{array}{rr} 
  \\hline 
  \\text{Time} & \\text{Index} \\\\ 
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
  \\text{Time} & \\text{Index} \\\\ 
  \\hline 
  \\text{1988} & 1.49 \\\\ 
  \\text{1989} & 1.51 \\\\
  \\text{1990} & 1.50 \\\\
  \\vdots & \\vdots \\\\ 
  \\text{2019} & 0.91 \\\\ 
  \\hline 
\\end{array}
$$
