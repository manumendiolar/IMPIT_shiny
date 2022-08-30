IMPIT data should be stored in a `CSV` file with four columns and header:

* The first three columns are for year, month and day. For monthly data, enter 1 in day column. For annual data, enter 1 in day and month columns, respectively. These values should be positive and integer. 
* The last column is for the IMPIT index value. This should be a numeric value. 

Please note that missing values are not allowed and space in header's names is not allowed either (e.g., use 'impit_index' instead of 'impit index').

*** 
An example of monthly data:

$$
\\scriptsize
\\begin{array}{rr}
  \\hline 
  \\text{Year} & \\text{Month} & \\text{Day} & \\text{Index} \\\\
  \\hline 
  \\text{1988} &  \\text{1} & \\text{1} &    1.49 \\\\
  \\text{1988} &  \\text{2} & \\text{1} &    1.51 \\\\
  \\text{1988} &  \\text{3} & \\text{1} &    5.03 \\\\
       \\vdots &    \\vdots &   \\vdots & \\vdots \\\\
  \\text{1988} & \\text{12} & \\text{1} &    0.91 \\\\
  \\hline 
\\end{array}
$$

