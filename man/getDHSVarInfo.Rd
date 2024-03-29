% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{getDHSVarInfo}
\alias{getDHSVarInfo}
\title{Get DHS variable information online.}
\usage{
getDHSVarInfo(browserLoc, variable)
}
\arguments{
\item{browserLoc}{a non-empty character string giving the name of the program to be used as the HTML browser.
It should be in the PATH, or a full path specified.}

\item{variable}{a non-empty character string giving the name of the variable the user wants to obtain metadata for.}
}
\value{
Opens the user specified web browser to variable metadata.
}
\description{
\code{getDHSVarInfo()} Opens a user specified web browser to metadata for a user selected variable on the IPUMS DHS website.
}
\examples{
GetDHSVarInfo(browserLoc = "C:/Program Files/Mozilla Firefox/firefox.exe", variable = 'CLUSTERNO')

}
