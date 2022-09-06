
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
library(jgNutritionTools)
```

## Examples

Assuming a valid DB connection has been made:

1.  Use helper function to display country year combinations

``` r
library(tidyverse)
## Show country and DHSYEAR combos for DHS surveys with country code
getCountryYearCom(mydb) %>% head()
#>   COUNTRY YEAR  countryName
#> 1     024 2015       Angola
#> 2     854 1999 Burkina Faso
#> 3     854 2003 Burkina Faso
#> 4     854 2010 Burkina Faso
#> 5     204 1996        Benin
#> 6     204 2001        Benin
```

2.  Retrieve CHIRPS precipitation data for Burkina Faso in 2003

``` r
getPrecip(conn = mydb, countryCode = "854",
          Year = "2003",
          startDate = '2002-01-01',
          endDate = '2003-12-31')[1:5, c(1:2, 23:28)]
#>   spatialPointsIDX          DHSID mm_month_Jan_2002 mm_month_Feb_2002
#> 1              836 BF200300000001        0.01418461        0.16241387
#> 2              837 BF200300000002        0.02902447        0.06427223
#> 3              838 BF200300000003        0.06426939        0.69320846
#> 4              839 BF200300000004        0.04322197        0.49001950
#> 5              840 BF200300000005        0.08005400        0.56486058
#>   mm_month_Mar_2002 mm_month_Apr_2002 mm_month_May_2002 mm_month_Jun_2002
#> 1         1.8029082          6.995065          35.49744          77.05067
#> 2         0.9449331          3.606222          27.18962          60.78111
#> 3         3.9485326         15.639685          35.19349         109.18393
#> 4         2.1929412          7.838782          39.50251          87.84183
#> 5         1.6934129         10.176802          35.68568          98.90775
```

3.  Retrieve CHIRTS maximum temperature data for Angola in 2015

``` r
getTMax(conn = mydb, countryCode = "024",
        Year = "2015",
        startDate = "2015-01-01",
        endDate = "2015-12-31")[1:5, c(1:2, 23:28)]
#>   spatialPointsIDX          DHSID tMax_Jan_2015 tMax_Feb_2015 tMax_Mar_2015
#> 1                1 AO201500000001      28.49672      29.13592      28.85665
#> 2                2 AO201500000002      29.55231      29.59307      29.35476
#> 3                3 AO201500000003      31.12279      31.56959      31.74590
#> 4                4 AO201500000004      27.78304      29.45940      28.18334
#> 5                5 AO201500000005      31.58098      31.80293      30.88173
#>   tMax_Apr_2015 tMax_May_2015 tMax_Jun_2015
#> 1      29.16747      28.93256      27.02500
#> 2      29.50359      30.53665      30.14890
#> 3      31.74549      30.38935      26.73691
#> 4      27.11741      26.93912      25.54288
#> 5      31.30521      30.61591      27.45424
```

## Acknowledgements

A portion of the source code for functions in the `jgNutritionTools`
package was developed using code from the
[ropensci/chirps](https://github.com/ropensci/chirps) package. We thank
these developers for providing this code under the MIT licence.
