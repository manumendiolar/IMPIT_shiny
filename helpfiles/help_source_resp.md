Source data for the response variable should be stored as a `CSV` format with four columns and header.

* The first three columns are for the year, month and day of the time variable, respectively. 
* The last column is for the response value (numeric value).
* Please note that missing values are not allowed, space in header's names is not allowed either (e.g., use 'resp_variable' instead of 'resp variable') and time range between IMPIT index and response variable should match.

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
An example of monthly data for the response variable:

$$
\\scriptsize
\\begin{array}{rr}
  \\hline 
  \\text{Year} & \\text{Month} & \\text{Day} & \\text{catch_rate} \\\\
  \\hline 
  \\text{1988} & \\text{1} & \\text{1} &   52.85 \\\\
  \\text{1989} & \\text{2} & \\text{1} &   61.20 \\\\
  \\text{1990} & \\text{2} & \\text{1} &   49.42 \\\\
       \\vdots &   \\vdots &   \\vdots & \\vdots \\\\
  \\text{2019} & \\text{2} & \\text{1} &   15.94 \\\\
  \\hline 
\\end{array}
$$
