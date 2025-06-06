% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_io.R
\name{read_csvs_by_pattern}
\alias{read_csvs_by_pattern}
\title{Read and combine CSV files matching specified patterns}
\usage{
read_csvs_by_pattern(
  directory_path,
  missing_vals,
  patterns = NULL,
  all_files = FALSE,
  clean_col_names = TRUE,
  guess_max = 5000,
  combine = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{directory_path}{Character. Path to the directory containing CSV files}

\item{missing_vals}{Character vector. Values to be treated as NA (required)}

\item{patterns}{List or character vector. Patterns to match files against.
Each pattern will match any file containing that string.
\itemize{
\item Single pattern: "dataf_version_" will match all files containing this exact phrase
\item Multiple patterns: c("genetics", "clean_") will match ANY file containing EITHER "genetics" OR "clean_"
\item Regular expressions can be used: "Genetic_v\\d+" will match files "Genetic_v1", "Genetic_v2", etc
}}

\item{all_files}{Logical. If TRUE, reads all CSV files in directory regardless of patterns}

\item{clean_col_names}{Logical. If TRUE, cleans column names using janitor::clean_names()}

\item{guess_max}{Integer. Maximum number of rows to use for guessing column types}

\item{combine}{Logical. If TRUE, combines all dataframes into one. If FALSE, returns a named list}

\item{verbose}{Logical. If TRUE, prints progress messages}
}
\value{
A combined dataframe or a named list of dataframes
}
\description{
Read and combine CSV files matching specified patterns
}
