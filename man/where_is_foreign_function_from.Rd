% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{where_is_foreign_function_from}
\alias{where_is_foreign_function_from}
\title{Which foreign package is a function being loaded from?}
\usage{
where_is_foreign_function_from(current_function, package_name, cache_this)
}
\arguments{
\item{current_function}{Name of the function we care about}

\item{package_name}{Package it is being called from}

\item{cache_this}{An environment -- used to handle the memoisation}
}
\value{
A character vector containing the namespaced function call or
the namespace INVALID for cases where we've failed.
}
\description{
Figure out which, if any, namespace a function is being loaded from (and
if it is private or public) and return the information.
}
