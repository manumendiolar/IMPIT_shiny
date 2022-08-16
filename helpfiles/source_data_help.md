Source data should be stored as a `CSV` format with four columns and header.

* The first three columns are for the year, month and day of the time variable, respectively. 
* The last column is for the environmental signal.
* Please note that missing values are not allowed and space in header's names is not allowed either (e.g., use 'soi_monthly' instead of 'soi monthly').

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
  \\text{year} & \\text{month} & \\text{day} & \\text{SOI} \\\\
  \\hline 
  \\text{1950} & \\text{01} & \\text{01} & 5.1 \\\\
  \\text{1950} & \\text{02} & \\text{01} & 17.6 \\\\
  \\vdots & \\vdots & \\vdots & \\vdots \\\\
  \\text{2022} & \\text{02} &  \\text{01} & 8.2 \\\\
  \\hline 
\\end{array}
$$
