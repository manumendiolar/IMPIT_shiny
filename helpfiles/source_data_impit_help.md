Source IMPIT data should be stored as a `CSV` format with four columns and header.

* The first three columns are for the year, month and day of the time variable, respectively. 
* The last column is for the IMPIT index value (numeric value).
* Please note that missing values are not allowed and space in header's names is not allowed either (e.g., use 'impit_index' instead of 'impit index').

***
CSV Format:

$$
\\scriptsize
\\begin{array}{cc} 
  \\hline 
  \\text{Year} & \\text{Month} & \\text{Day} & \\text{Value} \\\\
  \\hline       
  \\cdot & \\cdot & \\cdot &        \\cdot \\\\ 
  \\hline 
\\end{array}
$$


*** 
An example:

$$
\\begin{array}{rr}
  \\hline 
  \\text{Year} & \\text{Month} & \\text{Day} & \\text{Index} \\\\
  \\hline 
  \\text{1988} & \\text{01} & \\text{01} & 1.49 \\\\
  \\text{1989} & \\text{02} & \\text{01} & 1.51 \\\\
  \\vdots & \\vdots & \\vdots & \\vdots \\\\
  \\text{2019} & \\text{02} &  \\text{01} & 0.91 \\\\
  \\hline 
\\end{array}
$$

