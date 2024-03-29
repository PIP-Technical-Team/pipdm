% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_create_coverage_table.R
\name{db_create_coverage_table}
\alias{db_create_coverage_table}
\title{Create coverage tables}
\usage{
db_create_coverage_table(
  ref_year_table,
  pop_table,
  cl_table,
  incgrp_table,
  ref_years,
  digits = 2,
  special_countries = c("ARG", "CHN", "IDN", "IND", "SUR")
)
}
\arguments{
\item{ref_year_table}{data.table: Full interpolated means table. Output of
\code{db_create_ref_year_table()}.}

\item{pop_table}{data.table: A table with population data.}

\item{cl_table}{data.table: Country list table with all WDI countries.}

\item{incgrp_table}{data.table: Table with historical income groups for all WDI countries.}

\item{ref_years}{numeric: A vector with reference years.}

\item{digits}{numeric: The number of digits the returned coverage numbers are
rounded by.}

\item{special_countries}{character: A string with 3-letter country codes.}
}
\value{
list
}
\description{
Create a list of tables with coverage estimates at 1) regional, WLD, TOT and
2) income group levels.
}
