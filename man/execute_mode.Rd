% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/execute_script.R
\name{execute_mode}
\alias{execute_mode}
\title{Execute script for a given mode}
\usage{
execute_mode(
  .parquet = get_config("parquet"),
  .references = get_config("references"),
  .aggregation = get_config("aggregation"),
  .config = getOption("rcbms.config"),
  .excluded_cases = NULL
)
}
\arguments{
\item{.parquet}{parquet data object. The default is \code{get_config("parquet")}.}

\item{.references}{references object}

\item{.aggregation}{aggregation object}

\item{.config}{config object}
}
\description{
Execute script for a given mode
}
