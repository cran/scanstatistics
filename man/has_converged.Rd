% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{has_converged}
\alias{has_converged}
\title{Is the relative error between two numbers is less than the given tolerance?}
\usage{
has_converged(current, previous, tol = 0.01)
}
\arguments{
\item{current}{A scalar; the most recent value of the sequence.}

\item{previous}{A scalar; the second most recent value of the sequence, or a
reference value.}

\item{tol}{The tolerance, a positive scalar near zero.}
}
\description{
Given two consecutive numbers in a sequence, return \code{TRUE} if the
relative change is positive but less than the given tolerance.
}
\keyword{internal}
