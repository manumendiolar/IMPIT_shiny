Source data should be stored as a `CSV` format with two columns and header.

* The first column is for the time variable (could be year, month, day, etc.), and the second column for the environmental signal.
* Please note that missing values are not allowed and space in header's names is not allowed either (e.g., use 'soi_monthly' instead of 'soi monthly').

***
CSV Format:

$$\\scriptsize\\begin{array}{cc} \\hline \\text{Date} & \\text{SOI} \\\\ \\hline \\cdot & \\cdot \\\\ \\hline \\end{array}$$


*** 
An example:

$$\\begin{array}{rr} \\hline \\text{Date} & \\text{SOI}
\\\\ \\hline \\text{01/01/1950} & 5.1
\\\\ \\hline \\text{01/01/1950} & 17.6
\\\\ \\hline \\vdots & \\vdots
\\\\ \\hline \\text{01/02/2022} & 8.2 \\\\ \\hline \\end{array}$$
=======
Soruce data should be stored as a `CSV` format with two columns and header.

* The first column is for time (could be year, month, day, etc.), and the second column for the environmental signal.
* Please note that missing values are not allowed and neither space in header's names (e.g., use 'soi_monthly' instead of 'soi monthly').

***
CSV Format:

$$\\scriptsize\\begin{array}{cc} \\hline \\text{Date} & \\text{SOI} \\\\ \\hline \\cdot & \\cdot \\\\ \\hline \\end{array}$$


*** 
An example:

$$\\begin{array}{cc} \\hline \\text{Date} & \\text{soi_monthly}
\\\\ \\hline \\text{01/01/1950} & 5.1
\\\\ \\hline \\text{01/01/1950} & 17.6
\\\\ \\hline \\vdots & \\vdots
\\\\ \\hline \\text{01/02/2022} & 8.2 \\\\ \\hline \\end{array}$$
