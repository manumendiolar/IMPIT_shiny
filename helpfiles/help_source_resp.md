Response variable data should be stored in a `CSV` file with four columns and header:

* The first three columns are for year, month and day. For monthly data, enter 1 in day column. For annual data, enter 1 in day and month columns, respectively. These values should be positive and integer.  
* The last column is for the response value. This should be a numeric value. 

Please note that missing values are not allowed and space in header's names is not allowed either (e.g., use 'resp_variable' instead of 'resp variable').

*** 
An example of monthly data:

$$
\\scriptsize
\\begin{array}{rr}
  \\hline 
  \\text{Year} & \\text{Month} & \\text{Day} & \\text{catch_rate} \\\\
  \\hline 
  \\text{1988} &  \\text{1} & \\text{1} &   52.85 \\\\
  \\text{1988} &  \\text{2} & \\text{1} &   61.20 \\\\
  \\text{1988} &  \\text{3} & \\text{1} &   49.42 \\\\
       \\vdots &    \\vdots &   \\vdots & \\vdots \\\\
  \\text{1988} & \\text{12} & \\text{1} &   15.94 \\\\
  \\hline 
\\end{array}
$$
