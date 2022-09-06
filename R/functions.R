# GetDHSVarInfo ----
#' Get DHS variable information online.
#'
#' \code{getDHSVarInfo()} Opens a user specified web browser to metadata for a user selected variable on the IPUMS DHS website.
#'
#' @param browserLoc a non-empty character string giving the name of the program to be used as the HTML browser.
#'        It should be in the PATH, or a full path specified.
#' @param variable a non-empty character string giving the name of the variable the user wants to obtain metadata for.
#' @return Opens the user specified web browser to variable metadata.
#' @examples
#' GetDHSVarInfo(browserLoc = "C:/Program Files/Mozilla Firefox/firefox.exe", variable = 'CLUSTERNO')
#'
getDHSVarInfo <- function(browserLoc, variable){
  utils::browseURL(url = paste0("https://www.idhsdata.org/idhs-action/variables/",
                         variable, "#codes_section"),
            browser = browserLoc)
}

# uniqueID() ----
#' Find unique identifier for a specified dataframe
#'
#' \code{uniqueID()} When used with lapply on a data frame it counts the number of unique variables in each column
#'
#' @param x a data frame
#' @return The number of unique variables by column. The unique ID should equal the total number of observations in
#'          the dataframe.
#'
#' @examples lapply(dataframe, FUN = uniqueID)
#'
uniqueID <- function(x) {
  length(unique(x))
}

# check_db_size() ----
#' Check database size
#'
#' `check_db_size()` Shows the size of database in MB
#'
#' @param conn a Formal class MySQLConnection established with RMySQL
#' @param db a non-empty character string of a named database. Defaults to `'NutritionSecurity-2'`
#' @return A table showing the size of the database in megabytes
#'
#' @examples check_db_size(conn = mydb, db = NutritionSecurity)
#'
check_db_size <- function(conn, db = 'NutritionSecurity-2') {
  DBI::dbGetQuery(conn = conn,
             statement = paste0("SELECT table_schema '", db, "', ",
                                "ROUND(SUM(data_length + index_length) / 1024 / 1024, 1) 'DB Size in MB'
FROM information_schema.tables
GROUP BY table_schema"))
}




# getPrecip()----


#' Get CHIRPS data for a specified country and time period
#'
#' \code{getPrecip()} takes DHS spatial point data from the jgnutritionsecurity database and reads geotiff data from an HTTP web protocol to calculate monthly precipitation values at each point of interest.
#'
#' @param conn a Formal class MySQLConnection established with `RMySQL`
#' @param countryCode a 3 digit character vector representing the country where spatial points should be extracted from. This is the
#' 'COUNTRY' column in the countries table in the jgnutritionsecurity database. Run \code{\link{getCountryYearCom}} to find unique country and year combinations.
#' @param  Year a 4 digit character vector representing the Year the survey was conducted.
#' @param startDate,endDate YYYY-MM-DD for start date and end date.
#'
#' @return A data frame with monthly precipitation values in mm/month for each spatial observation. Additional attributes for spatial observations will be preserved.
#' See CHIRPS website for more information. \url{https://www.chc.ucsb.edu/data/chirps}
#' @examples
#' getPrecip(conn = mydb,
#'  countryCode = "024",
#'   Year = "2015",
#'    startDate = "2014-01-01",
#'    endDate = "2016-12-31")
#'
#' Will return precipitation data for all DHS points in
#' Angola from 01-01-2014 to 12-31-2016

getPrecip <- function(conn, countryCode, Year, startDate, endDate) {
  # If a package is installed, it will be loaded. If any
  # are not, the missing package(s) will be installed
  # from CRAN and then loaded.

  # # ## First specify the packages of interest
  # packages <- c("tidyverse", "stringr", "sf", "lubridate", "zoo")
  #
  # ## Now load or install&load all
  # package.check <- lapply(
  #   packages,
  #   FUN = function(x) {
  #     if (!require(x, character.only = TRUE)) {
  #       install.packages(x, dependencies = TRUE)
  #       library(x, character.only = TRUE)
  #     }
  #   }
  # )

  #  Extract spatial data from DB
  pointsDF <-
    DBI::dbGetQuery(
      conn = conn,
      statement = paste0(
        "SELECT * FROM spatialpoints WHERE COUNTRY = '",
        noquote(countryCode),
        "' AND YEAR = '",
        noquote(Year),
        "'"
      )
    )

  # pointsDF <-
  #   DBI::dbGetQuery(
  #     conn = pool,
  #     statement = "SELECT * FROM spatialpoints WHERE COUNTRY = '854' AND YEAR = '2003'")

  #Change class to sf
  #pointsSF <- sf::st_as_sf(pointsDF, coords = c("LONGNUM", "LATNUM"), remove = FALSE)

  #Set end Date and Start Dates
  # endDate <-
  #   lubridate::ymd(paste0(as.character(pointsDF$YEAR[1]), "-12-31"))
  # startDate <-
  #   lubridate::ymd(paste0(as.character(pointsDF$YEAR[1]), "-01-01")) %m-% years(as.integer(timeFrame))
  endDate <- lubridate::ymd(endDate)
  startDate <- lubridate::ymd(startDate)


  #Sequence to pull dates for download links from ftp
  seqdate <-
    format(seq.Date(zoo::as.Date(startDate), zoo::as.Date(endDate), by = "month"), format = "%Y-%m")
  years <-
    format(seq.Date(zoo::as.Date(startDate), zoo::as.Date(endDate), by = "year"), format = "%Y")

  #Set filename variable
  file_name_var <- "chirps-v2.0."

  #year range check for data availability
  yrange <-
    seq(1981, as.numeric(format(Sys.Date(), format = "%Y")), 1)
  stopifnot("Some dates entered are outside of CHIRTS data temporal coverage" = unique(years) %in% yrange)
  #format dates to match dates in http address for .tiff files
  dates <- gsub("-", "\\.", seqdate)
  fnames <- file.path(paste0(file_name_var , dates, ".cog"))
  # set file path to chirps location
  # cog =  cloud optomized geotiff. A more efficient way to access geospatial data online
  u <-
    file.path("https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/cogs",
              fnames)
  # use gdal vsicurl to increase efficiency
  u1 <- file.path("/vsicurl", u)
  # download rasters
  r <- terra::rast(u1)
  # use terra extract to extract raster values to points
  rr <- terra::extract(r, pointsDF[, c("LONGNUM", "LATNUM")], xy = F)
  # rename columns
  dates <-
    gsub(pattern = "\\.", "-", names(rr)[grep("chirps-v2.0.", names(rr))]) %>%
    stringr::str_remove("chirps-v2-0-") %>%
    zoo::as.yearmon()
  colnames(rr)[grep("chirps-v2.0.", names(rr))] <-
    paste0("mm_month_", gsub("[^[:alnum:]]", "_", dates))
  # left join to original spatial data frame by long and lat
  # rr <-
  #   left_join(
  #     x = pointsDF,
  #     y = rr[, 2:length(rr)],
  #     by = c("LATNUM" = "y", "LONGNUM" = "x")
  #   )
  #bind to original dataframe
  rr <- cbind(pointsDF, rr[2:length(rr)])
  # return data
  return(rr)
}

# getTmax()----
#' Get CHIRTSmonthly data for a specified country and time period
#'
#' \code{getTmax()} takes DHS spatial point data from the jgnutritionsecurity database and reads geotiff data from an HTTP web protocol to calculate monthly temperature values at each point of interest.
#'
#' @param conn a Formal class MySQLConnection established with `RMySQL`
#' @param countryCode a 3 digit character vector representing the country where spatial points should be extracted from. This is the
#' 'COUNTRY' column in the countries table in the jgnutritionsecurity database. Run \code{\link{getCountryYearCom}} to find unique country and year combinations.
#' @param  Year a 4 digit character vector representing the Year the survey was conducted.
#' @param startDate,endDate YYYY-MM-DD for start date and end date.
#'
#' @return A data frame with monthly CHIRTSmax values in Celsius for each spatial observation. Additional attributes for spatial observations will be preserved.
#' See CHIRTS website for more information. \url{https://www.chc.ucsb.edu/data/chirtsmonthly}
#'
#' @examples
#' getTMax(conn = mydb,
#'  countryCode = "024",
#'   Year = "2015",
#'    startDate = "2014-01-01",
#'    endDate = "2016-12-31")
#'
#' Will return monthly CHIRTSmax data for all DHS points in
#' Angola from 01-01-2014 to 12-31-2016
#'

getTMax <- function(conn, countryCode, Year, startDate, endDate) {
#  packages <- c("tidyverse", "stringr", "sf", "lubridate", "zoo")
#
#   ## Now load or install&load all
#   package.check <- lapply(
#     packages,
#     FUN = function(x) {
#       if (!require(x, character.only = TRUE)) {
#         install.packages(x, dependencies = TRUE)
#         library(x, character.only = TRUE)
#       }
#     }
#   )
  ##Extract spatial data from DB
  pointsDF <-
    DBI::dbGetQuery(
      conn = conn,
      statement = paste0(
        "SELECT * FROM spatialpoints WHERE COUNTRY = '",
        noquote(countryCode),
        "' AND YEAR = '",
        noquote(Year),
        "'"
      )
    )
  #Change class to sf
  #pointsSF <- sf::st_as_sf(pointsDF, coords = c("LONGNUM", "LATNUM"), remove = FALSE)

  #Set end Date and Start Dates
  endDate <- lubridate::ymd(endDate)
  startDate <- lubridate::ymd(startDate)

  #Sequence to pull dates for download links from ftp
  seqdate <-
    format(seq.Date(zoo::as.Date(startDate), zoo::as.Date(endDate), by = "month"), format = "%Y-%m")
  years <-
    format(seq.Date(zoo::as.Date(startDate), zoo::as.Date(endDate), by = "year"), format = "%Y")

  #Set filename variable
  file_name_var <- "CHIRTSmax."

  #year range check for data availability
  yrange <- seq(1983, 2016, 1)
  stopifnot("Dates entered are outside of CHIRTS data temporal coverage" = unique(years) %in% yrange)

  dates <- gsub("-", "\\.", seqdate)
  fnames <- file.path(paste0(file_name_var , dates, ".tif"))

  #resolution <- gsub("0\\.", "p", resolution)

  u <-
    file.path("https://data.chc.ucsb.edu/products/CHIRTSmonthly/CHIRTSmax.CDR",
              fnames)

  u1 <- file.path("/vsicurl", u)
  # download rasters
  r <- terra::rast(u1)
  # use terra extract to extract raster values to points
  rr <- terra::extract(r, pointsDF[, c("LONGNUM", "LATNUM")], xy = F)
  # rename columns
  dates <-
    gsub(pattern = "\\.", "-", names(rr)[grep("CHIRTSmax.", names(rr))]) %>%
    stringr::str_remove("CHIRTSmax-") %>%
    zoo::as.yearmon()

  colnames(rr)[grep("CHIRTSmax.", names(rr))] <-
    paste0("tMax_", gsub("[^[:alnum:]]", "_", dates))
  # left join to original spatial data frame by long and lat
  # rr <-
  #   left_join(
  #     x = pointsDF,
  #     y = rr[, 2:length(rr)],
  #     by = c("LATNUM" = "y", "LONGNUM" = "x")
  #   )
  rr <- cbind(pointsDF, rr[2:length(rr)])
  # return data
  return(rr)
}

# getCountryYearCom()----
#' Display all combinations of country and year for DHS surveys.
#'
#' \code{getCountryYearCom()} will display all combinations of country and year for DHS surveys
#'
#' @param conn a Formal class MySQLConnection established with DBI and RMySQL
#' @return a data frame will the unique combinations of country and year for DHS surveys.
#' used as a helper for running functions like `get_tMax()` and `getPrecip()`.
#' @examples
#' showCountryYearCom(conn = mydb)
#'
#'
getCountryYearCom <- function(conn) {
  DBI::dbGetQuery(
    conn,
    statement = "SELECT p.COUNTRY, YEAR, countryName
    FROM spatialpoints p
    INNER JOIN countries c
    ON c.COUNTRY = p.COUNTRY
    GROUP BY p.COUNTRY, YEAR;"
  )
}

