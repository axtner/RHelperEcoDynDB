% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSQL.r
\name{getSQL}
\alias{getSQL}
\title{helper function to format and run existing SQL scripts from R}
\usage{
getSQL(filepath)
}
\arguments{
\item{filepath}{File path to SQL script.}
}
\description{
getSQL
}
\details{
Function to format and run SQL scripts from R. Use DBI::dbGetQuery(db_con, getSQL("CHANGEthisFILE.sql"))
}
