% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tracking.R
\name{register_processed}
\alias{register_processed}
\title{Register variables as processed}
\usage{
register_processed(func_name, vars)
}
\arguments{
\item{func_name}{String. Name of the processing function}

\item{vars}{Character vector. Names of variables being processed}
}
\value{
Invisible NULL
}
\description{
Marks a set of variables as having been processed by a specific function.
}
