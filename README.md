# `IMPIT-a` <img src="www/images/icon_explore_2.svg" width="25" height="25">
IMPIT-a provides a smooth workflow from importing raw signal data, exploring and defining episodes, to constructing IMPIT indices. It also offers a suite of tools for index exploration before exporting for posterior use. IMPIT-a is built in [R](https://www.r-project.org) with an package [Shiny](https://shiny.rstudio.com), a web application framework for R.  [RStudio](https://www.rstudio.com) provides an integrated environment for R. We suggest users can run IMPIT-a in Rstudio.

-----

## Install IMPIT-a

Copy and paste the code to the R console in RStudio to download the IMPIT-a code from github. *The [shiny](https://shiny.rstudio.com/) library must be installed.*

``` r
if(!require("devtools"))  install.packages("devtools")
shiny::runGitHub("manumendiolar/IMPIT_shiny")
```

#### Other Dependencies

The app also relies on the following packages,
[`shinydashboard`](https://rstudio.github.io/shinydashboard/index.html),
[`shinydashboardPlus`](https://rinterface.github.io/shinydashboardPlus/),
[`shinyFiles`](https://github.com/thomasp85/shinyFiles),
[`shinyhelper`](https://github.com/cwthom/shinyhelper),
[`shinyalert`](https://github.com/daattali/shinyalert),
[`shinyvalidate`](https://rstudio.github.io/shinyvalidate/),
[`shinyjs`](https://deanattali.com/shinyjs/)
[`shinyWidgets`](https://github.com/dreamRs/shinyWidgets),
[`dashboardthemes`](https://cran.r-project.org/web/packages/dashboardthemes/index.html),
[`tidyverse`](https://www.tidyverse.org/),
[`DT`](https://rstudio.github.io/DT/),
[`plotly`](https://plotly.com/r/),
[`spsComps`](https://github.com/lz100/spsComps) and
[`lubridate`](https://lubridate.tidyverse.org/)

To install these packages:
``` r
# Run it in R
get.packages <- function (){
  pkglist = c("shinydashboard", "shinydashboardPlus", "shinyFiles","shinyhelper",
              "shinyalert", "shinyvalidate",,"shinyjs""dashboardthemes", "shinyWidgets",
              "tidyverse","DT","plotly","spsComps","lubridate")
  inst.pkgs = rownames(installed.packages())
  newpkgs <- pkglist[!pkglist %in% inst.pkgs]
  if (length(newpkgs) > 0) {
    do.call("install.packages", list(pkglist))
  }
}

get.packages()
```

### Use the website

Alternatively, you can also access the app via <https://manumendiolar.shinyapps.io/impit_shiny/>


### Demo data 

A list of example .csv files have been provided. Please use this [link to download](https://github.com/manumendiolar/IMPIT_shiny/tree/main/example-data) the files. 

-----

<!--## R Package

The code located in [Rlib](/Rlib) contains an R package named
`dublinRTPI`. This can be installed using the `devtools` package.

The package contains functions to retrieve live info for Dart and Dublin
Bus. A light version of the main shiny app is also included in the
package.

``` r
# install.packages("devtools")
devtools::install_github("manumendiolar/IMPIT_shiny", subdir = "Rlib")

 # Get info about bus stop number 334
dublinRTPI::db_info(334)

 # Run shiny app
dublinRTPI::runShiny()
```




<!--### Input variables for **IMPIT-a**

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
-->

## Further reading

* Mendiolar, M., Filar, J. A., Yang, W. H., Leahy, S., & Courtney, A. (2023). Capturing episodic impacts of environmental signals. arXiv preprint arXiv:2303.16073. [link](https://arxiv.org/abs/2303.16073)

## About Authors

<img src="www/images/UQ_logo.png" width="150"/> 

[Manuela Mendiolar](https://smp.uq.edu.au/profile/8282/manuela-mendiolar) is currently a PhD candidate at the [University of Queensland's](https://www.uq.edu.au/) School of Mathematics and Physics.

<!--
## Copyright
[![License](https://img.shields.io/badge/Licence-GPL%20v2.0-orange.svg)](link)
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
