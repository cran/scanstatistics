% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scanstatistics_general.R
\name{score_locations}
\alias{score_locations}
\title{Score each location over zones and duration.}
\usage{
score_locations(x, zones)
}
\arguments{
\item{x}{An object of class \code{scanstatistic}.}

\item{zones}{A list of integer vectors.}
}
\value{
A \code{data.table} with the following columns:
   \describe{
     \item{location}{The locations (as integers).}
     \item{total_score}{For each location, the sum of all window statistics 
                        that the location appears in.}
     \item{n_zones}{The number of spatial zones that the location appears 
                    in.}
     \item{score}{The total score divided by the number of zones and the 
                  maximum duration.}
     \item{relative_score}{The score divided by the maximum score.}
}
}
\description{
For each location, compute the average of the statistic calculated for each
space-time window that the location is included in, i.e. average the 
statistic over both zones and the maximum duration.
}
\examples{
# Simple example
set.seed(1)
table <- data.frame(zone = 1:5, duration = 1, score = 5:1)
zones <- list(1:2, 1:3, 2:5, 4:5, c(1, 5))
x <- list(observed = table, n_locations = 5, max_duration = 1, n_zones = 5)
score_locations(x, zones)
}
