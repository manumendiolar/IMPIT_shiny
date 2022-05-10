# `IMPIT-a` <img src="www/images/icon_explore_2.svg" width="10" height="10">
IMPIT-a is an application that provides a smooth workflow from importing raw data, exploring and defining episodes, to constructing IMPIT indices. It allows the user to choose between a menu of intensity and relative weights functions. It also offers the possibility to visualize imported data, defined episodes and construced IMPIT indices. 
<br>
<br>

# Prerequisites for using IMPIT-a
IMPIT-a was built in [R](https://www.r-project.org), an open source programming language using the [Shiny package](https://shiny.rstudio.com), a web application framework for R. Users will need to download [R](https://cran.uni-muenster.de/) in order to use IMPIT-a and we suggest the use of [RStudio](https://www.rstudio.com). R is completely free to use. All required code can be found in this github repository.



# Launch the Shiny app

blah blah


### Input variables for **IMPIT-a**

#### Environmental signal data
| Variable             	| Detail                                                                           	|
|----------------------	|----------------------------------------------------------------------------------	|
| ddtime | Time variable. Could be year, month or day and it should be in the format YYYY-MM-DD. |
| EnvSignal | Environmental signal |

#### Episodes data
| Variable             	| Detail                                                                           	|
|----------------------	|----------------------------------------------------------------------------------	|
| event_no | Number to identify the event / Unique identifier for the event. |
| duration | Duration of the event. |
| date_start | Date when the event starts. |
| date_peak | Date when the event attains its maximum intensity. |
| date_end | Date when the event ends. |
| intensity_mean | Mean value. |
| intensity_median | Median value.  |
| intensity_max | Maximum value. |
| intensity_min | Minimum value.  |
| intensity_log | Natural logarithm of the sum. |


#### IMPIT index data
| Variable             	| Detail                                                                           	|
|----------------------	|----------------------------------------------------------------------------------	|
| Memory | Time window memory. |
| Intensity | Intensity function. |
| $a$ | Parameter associated with *Persistence* importance weight.|
| $b$ | Parameter associated with *Recency* importance weight. Dampening parameter. |
| $c$ | Parameter associated with *Recency* importance weight. |
| $d$ | Parameter associated with *Timing* importance weight. |


# Further reading

* Add the published paper here.
* Add the FRDC report?

# Authors
<br>
![UQ_logo.png](www/images/UQ_logo.png){width=30%}
<br>
<br>
IMPIT-a was created at the School of Mathematics and Physics of the [University of Queensland](https://www.uq.edu.au/) by [Manuela Mendiolar](https://smp.uq.edu.au/profile/8282/manuela-mendiolar), PhD Student.

## Copyright
<!--[![License](https://img.shields.io/badge/Licence-GPL%20v2.0-orange.svg)](link)
IMPIT-a is licensed under the [GNU General Public License (GPL) v2.0](link). In a nutshell, this means that this package:

- May be used for commercial purposes

- May be used for private purposes

- May be modified, although:

  - Modifications **must** be released under the same license when distributing the package
  - Changes made to the code **must** be documented

- May be distributed, although:

  - Source code **must** be made available when the package is distributed
  - A copy of the license and copyright notice **must** be included.

- Comes with a LIMITATION of liability

- Comes with NO warranty-->
