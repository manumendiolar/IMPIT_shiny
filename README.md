# `IMPIT-a` <img src="www/images/icon_explore_2.svg" width="25" height="25">
Provides a smooth workflow from importing raw data, exploring and defining episodes, to constructing IMPIT indices. Offers, as well, a suite of tools for index exploration before exporting it for posterior use. IMPIT-a was built in [R](https://www.r-project.org), an open source programming language using the [Shiny package](https://shiny.rstudio.com), a web application framework for R. Users will need to download [R](https://cran.uni-muenster.de/) in order to use IMPIT-a and we suggest the use of [RStudio](https://www.rstudio.com). R is completely free to use. All required code can be found in this github repository.

-----

## Shiny App

The code located is located in [ShinyApp](/https://github.com/manumendiolar/IMPIT_shiny).

A live version of the app should be running at
<https://manumendiolar.shinyapps.io/impit_shiny/?_ga=2.87453400.1164059912.1657767117-1978399315.1647491284>

### Run Locally

The Shiny app can be run locally from within R or R Studio. *The [shiny](https://shiny.rstudio.com/) library must be installed.*

``` r
# install.packages("shiny")
shiny::runGitHub("manumendiolar/IMPIT_shiny")
```

#### Other Dependencies

The app also relies on the following packages,
[`shinydashboard`](https://dplyr.tidyverse.org/),
[`shinydashboardPlus`](https://cran.r-project.org/web/packages/XML/index.html),
[`shinyFiles`](https://cran.r-project.org/web/packages/rvest/),
[`shinyhelper`](https://cran.r-project.org/web/packages/rvest/),
[`dashboardthemes`](https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html),
[`tidyverse`](https://cran.r-project.org/web/packages/rvest/),
[`DT`](https://cran.r-project.org/web/packages/rvest/) and
[`plotly`](https://cran.r-project.org/web/packages/rvest/).


To install these packages:
``` r
# Run it in R
get.packages <- function (){

  pkglist = c("shinydashboard", "shinydashboardPlus", "shinyFiles", "DT",
              "shinyhelper", "dashboardthemes", "tidyverse", "plotly")

  inst.pkgs = rownames(installed.packages())

  newpkgs <- pkglist[!pkglist %in% inst.pkgs]

  if (length(newpkgs) > 0) {
    do.call("install.packages", list(pkglist))
  }
}

get.packages()
```
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
-->

# Further reading

* Add the published paper here.
* Add the FRDC report?

# Authors

<img src="www/images/UQ_logo.png" width="250"/> 

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
