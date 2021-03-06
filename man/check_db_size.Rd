% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{check_db_size}
\alias{check_db_size}
\title{Check database size}
\usage{
check_db_size(connection, db)
}
\arguments{
\item{db}{a non-empty character string of a named database}

\item{conn}{a Formal class MySQLConnection established with RMySQL}
}
\value{
A table showing the size of the database in megabytes
}
\description{
`check_db_size()` Shows the size of database in MB
}
\examples{
check_db_size(conn = mydb, db = NutritionSecurity)

}
