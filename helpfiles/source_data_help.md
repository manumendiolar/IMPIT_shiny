Source data should be stored as a `CSV` format with four columns and header.

* The first three columns are for the year, month and day of the time variable, respectively. For monthly data, assign a 1 for day column. For annual data, assign a 1 for month and day columns.  
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
An example of daily data:

$$
\\scriptsize
\\begin{array}{rr}
  \\hline 
  \\text{year} & \\text{month} & \\text{day} & \\text{river_discharge} \\\\
  \\hline 
  \\text{1958} & \\text{10} &   \\text{2} &  527.16 \\\\
  \\text{1958} & \\text{10} &   \\text{3} &  549.27 \\\\
  \\vdots      & \\vdots    &   \\vdots   & \\vdots \\\\
  \\text{1958} & \\text{10} &  \\text{31} &  343.46 \\\\
  \\hline 
\\end{array}
$$

An example of monthly data:

$$
\\scriptsize
\\begin{array}{rr}
  \\hline 
  \\text{year} & \\text{month} & \\text{day} & \\text{SOI} \\\\
  \\hline 
  \\text{1950} &  \\text{1} &   \\text{1} &     5.1 \\\\
  \\text{1950} &  \\text{2} &   \\text{1} &    17.6 \\\\
  \\vdots      &    \\vdots &     \\vdots & \\vdots \\\\
  \\text{2022} & \\text{12} &  \\text{1} &     8.2 \\\\
  \\hline 
\\end{array}
$$

An example of annual data:

$$
\\scriptsize
\\begin{array}{rr}
  \\hline 
  \\text{year} & \\text{month} & \\text{day} & \\text{SOI} \\\\
  \\hline 
  \\text{1970} & \\text{1} & \\text{1} &     5.1 \\\\
  \\text{1971} & \\text{1} & \\text{1} &    17.6 \\\\
  \\vdots      &   \\vdots & \\vdots   & \\vdots \\\\
  \\text{2011} & \\text{1} & \\text{1} &     8.2 \\\\
  \\hline 
\\end{array}
$$
