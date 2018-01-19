#' Maps relationships between functions in a package.
#'
#' @param package_name a character string containing the name of an installed package
#' @return A list of each function in the package, each either NULL or
#' a vector of character strings referring to which other functions the function
#' calls.
#' @export
explore_package = function(package_name) {
  # Create a new global cache environment to proto-memoise the package function
  # lookups.
  cache_this = new.env()

  # Get every function
  function_results = get_functions_from_package(package_name, cache_this)
  all_functions = c(function_results$public, function_results$private)

  # Every function we need to chase down the rabbit whole
  encoded_functions = unname(sapply(all_functions,
                             function(x) { paste0(package_name,
                                                  ifelse(x %in% function_results$public,
                                                         "::",
                                                         ":::"),
                                                  name_need_quote(x)) }))

  # Figure out which functions each function calls
  results = sapply(encoded_functions,
                   get_calls_from_function,
                   all_functions,
                   package_name,
                   cache_this)

  return(results)
}

#' Generates an adjacency matrix between functions in a package
#'
#' @param package_name a character string containing the name of an installed package
#' @param coerce_to_matrix If TRUE, return an R matrix instead of a sparse Matrix.
#' @return A square matrix where each row and column are functions in the package.
#' The matrix is an adjacency function: a 1 in a cell indicates that the function named
#' in the row calls the function named in the column.
#' @importFrom Matrix Matrix
#' @export
adjacency_matrix = function(package_name, coerce_to_matrix = FALSE) {
  results = explore_package(package_name)
  encoded_functions = names(results)

  # Build the adjacency matrix.
  adjacency_matrix = Matrix(0,
                            ncol = length(encoded_functions),
                            nrow = length(encoded_functions),
                            sparse = TRUE)
  colnames(adjacency_matrix) = rownames(adjacency_matrix) = encoded_functions
  for(row_name in encoded_functions) {
    for(column_name in results[[row_name]]) {
      if(column_name %in% encoded_functions) {
        adjacency_matrix[row_name, column_name] = 1
      }
    }
  }

  if(coerce_to_matrix) { return(as.matrix(adjacency_matrix)) }
  return(adjacency_matrix)
}
