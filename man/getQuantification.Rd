% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getQuantification.r
\name{getQuantification}
\alias{getQuantification}
\title{Read in data of PCRs from standardized quantification xls-files exported from a TECAN plate reader}
\usage{
getQuantification(in_dir = NA, pcr_batches = NULL)
}
\arguments{
\item{in_dir}{Folder containing the PCR info xls-files. If not provided the user is requested select one.}

\item{pcr_batches}{Mandatory integers that characterizing the PCR batches that were quantified in the respective xls-files. For example "pcr_batches =c(1:5)" will query the database for the samples of the PCR batches "p001", "p002", "p003", "p004" and "p005", "pcr_batches =c(1, 25, 305)" will query for the PCR batches "p001", "p025" and "p305".}
}
\description{
getQuantification
}
\details{
Function to read standardized quantification xls-files. The relevant data is written into the EcoDyn database and some tables are updated.
}
