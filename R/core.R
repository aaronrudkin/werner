#' Maps relationships between functions in a package.
#'
#' This function takes the name of an R package and explores each function
#' within that package to map which other functions that function calls. This
#' creates "dependency graph" between the functions, which can allow you to
#' quickly understand how a package fits together, and also diagnose pathologies
#' in package code like excessive or unwarranted coupling between functions or
#' orphaned functions. The returned data is a list.
#'
#' @param package_name a character string containing the name of an installed
#' package
#' @param show_progress a boolean indicating whether to display a progress bar.
#' @return A list of each function in the package, each either NULL or
#' a vector of character strings referring to which other functions the function
#' calls.
#' @export
explore_package = function(package_name,
                           show_progress=FALSE) {
  # Create a new global cache environment to proto-memoise the package function
  # lookups.
  cache_this = new.env()
  cache_this[["show_progress"]] = show_progress

  # Get every function
  function_results = get_functions_from_package(package_name, cache_this)
  all_functions = c(function_results$public, function_results$private)

  # Namespace the functions according to whether they are exported.
  encoded_functions = unname(vapply(
    all_functions,
    function(x) {
      paste0(
        package_name,
        ifelse(x %in% function_results$public,
          "::",
          ":::"
        ),
        name_need_quote(x)
      )
    },
    ""
  ))

  # Figure out which functions each function calls by calling
  # get_calls_from_function for each function.
  results = sapply(encoded_functions,
                   get_calls_from_function,
                   all_functions,
                   package_name,
                   cache_this)

  # Close off progress bar
  clear_progress_bar(cache_this)

  # Class the results so I can use S3 methods later.
  class(results) = "werner_call_list"

  return(results)
}

#' Generates an adjacency matrix between functions in a package
#'
#' This function takes the name of an R package and explores each function
#' within that package to map which other functions that function calls. This
#' creates "dependency graph" between the functions, which can allow you to
#' quickly understand how a package fits together, and also diagnose pathologies
#' in package code like excessive or unwarranted coupling between functions or
#' orphaned functions. The returned data is a matrix, by default a sparse
#' `Matrix` from the `Matrix` package.
#'
#' @param package_name a character string containing the name of an installed
#' package
#' @param coerce_to_matrix If TRUE, return an R `matrix` instead of a sparse
#' `Matrix`.
#' @param show_progress a boolean indicating whether to display a progress
#' bar.
#' @return A square matrix where each row and column are functions in the
#' package. The matrix is an adjacency function: a 1 in a cell indicates that
#' the function named in the row calls the function named in the column.
#' @importFrom Matrix Matrix
#' @export
adjacency_matrix = function(package_name,
                            coerce_to_matrix = FALSE,
                            show_progress = FALSE) {

  # Get the raw data
  results = explore_package(package_name, show_progress=show_progress)
  encoded_functions = names(results)

  # Build the adjacency matrix.
  adjacency_matrix = Matrix(0,
                            ncol = length(encoded_functions),
                            nrow = length(encoded_functions),
                            sparse = TRUE)
  colnames(adjacency_matrix) = rownames(adjacency_matrix) = encoded_functions
  # Convert the list format to the cell-wise adjacency matrix format
  for(row_name in encoded_functions) {
    for(column_name in results[[row_name]]) {
      if(column_name %in% encoded_functions) {
        adjacency_matrix[row_name, column_name] = 1
      }
    }
  }

  # Sparse Matrix or Matrix?
  if(coerce_to_matrix) { return(as.matrix(adjacency_matrix)) }
  return(adjacency_matrix)
}
