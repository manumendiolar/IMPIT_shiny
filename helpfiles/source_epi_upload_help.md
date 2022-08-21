Source data should be stored as a `CSV` format with a minimum of 6 columns and header.

* The first column is a number for the event indicator. Use header: event_no.
* The second column is for the duration of the event (units according to user's choice). Use header: duration
* The third column is for the date when the event starts (date_start, format YYYY-MM-DD). Use header: date_start.
* The fourth column is for the date when the event peaks its maximum value (date_peak, format YYYY-MM-DD). Use header: date_peak.
* The fifth column is for the date when the event ends (date_end, format YYYY-MM-DD). Use header: date_end.
* The sixth column is for the intensity value (should be a numeric value). Header should start with: intensity_. For example: intensity_mean.


* Please note that missing values are not allowed and space in header's names is not allowed either (e.g., use 'date_start' instead of 'date start').

***
CSV Format:

$$
\\scriptsize
\\begin{array}{cccccccccc} 
  \\hline 
  \\text{event_no} & \\text{duration} & \\text{date_start} & \\text{date_peak} & \\text{date_end} & \\text{intensity_mean} & \\text{intensity_median} & \\text{intensity_max} & \\text{intensity_min} & \\text{intensity_log}\\\\ 
  \\hline 
  \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot \\\\ 
  \\hline 
\\end{array}
$$


*** 
An example:

$$
\\scriptsize
\\begin{array}{cccccccccc} 
  \\hline 
  \\text{event_no} & \\text{duration} & \\text{date_start} & \\text{date_peak} & \\text{date_end} & \\text{intensity_mean} & \\text{intensity_median} & \\text{intensity_max} & \\text{intensity_min} & \\text{intensity_log}\\\\ 
  \\hline
  \\text{1} & \\text{2} & \\text{1900-06-01} & \\text{1900-06-01} & \\text{1900-07-01} & \\text{18.05} & \\text{18.05} & \\text{26.10} & \\text{10.00} & \\text{3.59}\\\\ 
  \\text{2} & \\text{1} & \\text{1901-03-01} & \\text{1901-03-01} & \\text{1901-03-01} & \\text{9.40} & \\text{9.40} & \\text{9.40} & \\text{9.40} & \\text{2.24}\\\\ 
  \\text{3} & \\text{3} & \\text{1901-06-01} & \\text{1901-06-01} & \\text{1901-08-01} & \\text{14.67} & \\text{14.60} & \\text{19.60} & \\text{9.80} & \\text{3.78}\\\\ 
  \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot \\\\ 
  \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot \\\\ 
  \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot \\\\ 
  \\text{151} & \\text{1} & \\text{2022-02-01} & \\text{2022-02-01} & \\text{2022-02-01} & \\text{8.20} & \\text{8.20} & \\text{8.20} & \\text{8.20} & \\text{2.10}\\\\ 
  \\hline
\\end{array}
$$
