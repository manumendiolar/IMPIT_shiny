This provides a definition of episodes based on the threshold-crossing indices. To start generating episodes you need to:

* First, choose if you want to generate episodes above or below certain threshold. If above, tick up-episode checkbox. If below, tick down-episodes checkbox.
* Second, enter a value for the threshold.
* Thirdly, enter a value for the minimum duration of episodes (by default it is 1)for each episode. Minimum duration means, minimum consecutive values above or below the threshold.
* Finally, check yes or no for Timing to be accounted. If you tick yes you'll have to provide a day and month for both start and end of the special season of Timing.  

This app generates a list of episodes with the following format:

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

If Timing yes is selected, three columns will be added to the table above:   

$$
\\scriptsize
\\begin{array}{ccccccccccccc} 
  \\hline 
  \\text{event_no} & \\text{duration} & \\text{date_start} & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\text{overlap} & \\text{overlap_days} & \\text{overlap_months}\\\\ 
  \\hline 
  \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot & \\cdot \\\\ 
  \\hline 
\\end{array}
$$
*** 
An example (without Timing):

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
