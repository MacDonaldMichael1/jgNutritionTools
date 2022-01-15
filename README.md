
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jgNutritionTools

<!-- badges: start -->
<!-- badges: end -->

The goal of jgNutritionTools is to provide a toolset for the analysis
and management of human health and physical data.

## Installation

You can install the development version of jgNutritionTools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MacDonaldMichael1/jgNutritionTools")
```

## Examples

Assuming a valid DB connection has been made:

1.  Use helper function to display country year combinations

``` r
library(jgNutritionTools)
library(tidyverse)
## Show country and DHSYEAR combos for DHS surveys with country code
getCountryYearCom(mydb) %>% head()
#>   COUNTRY DHSYEAR  countryName
#> 1     024    2015       Angola
#> 2     854    1999 Burkina Faso
#> 3     854    2003 Burkina Faso
#> 4     854    2010 Burkina Faso
#> 5     204    1996        Benin
#> 6     204    2001        Benin
```

2.  Retrieve CHIRPS precipitation data for Burkina Faso in 2003

``` r
getPrecip(conn = mydb, countryCode = "854",
          Year = "2003",
          timeFrame = 0)
```

## Acknowledgements

A portion of the source code for functions in the `jgNutritionTools`
package was developed using code from the
[ropensci/chirps](https://github.com/ropensci/chirps) package. We thank
these developers for providing this code under the MIT licence.
