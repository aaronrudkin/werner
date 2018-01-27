#' Diagnoses werner's failures to detect foreign functions.
#'
#' This function paradoxically subsets werner's returns to only include
#' function calls it specifically could not resolve; the intent here is to aid
#' werner package development by identifying edge cases that are not yet
#' supported.
#'
#' @param package_name Name of a package to explore
#' @returns List where each key is a function in the package and values are
#' a vector of character strings for the function calls that could not be
#' resolved.
diagnose_werner_failures = function(package_name) {
  # Explore the package
  z = explore_package(package_name)

  # Drop keys with no calls
  z[vapply(z, is.null, logical(1))] = NULL

  # Subset to just keys with INVALID calls
  zz = lapply(z, function(x) {
    x[grepl("^INVALID::", x)]
  })

  # Nuke the remaining ones, which are character(0)
  zz[vapply(zz, length, numeric(1)) == 0] = NULL

  return(zz)
}
