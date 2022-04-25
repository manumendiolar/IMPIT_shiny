Source data should be stored as a `CSV` format with 10 columns and header.

* The first column is a number for the event indicator (event_no).
* The second column is for the duration of the event (units according to user's choice).
* The third column is for the date when the event start (date_start, format YYYY-MM-DD).
* The first column is for the time variable (could be year, month, day, etc.), and the second column for the environmental signal.
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
