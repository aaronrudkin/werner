#' Captures whether an R language statement is the assignment of a function
#' call.
#'
#' @param line An R statement
#' @param function_name The name of the function that this line is in.
#' @param cache_this An environment to cache the results so this does not need
#' to be a two-pass parser
#' @importFrom rlang is_lang lang_head lang_tail is_primitive lang_fn
capture_internal_function_def = function(line, function_name, cache_this) {
  # If it's not a language call, it shouldn't be here.
  if(!is_lang(line)) {
    return()
  }

  # Verify that this is either <- or = as a function
  if(deparse(lang_head(line))[[1]] %in% c("<-", "=")) {

    # variable_name = assigned_to()
    variable_name = deparse(lang_tail(line)[[1]])

    # If the assignment is a private function of the package, the `lang_fn`
    # extraction will fail -- but this is OK, because that means it's not
    # a function definition, clearly.
    assigned_to = tryCatch({
      assigned_to_temp = lang_fn(lang_tail(line)[[2]])
      assigned_to_temp
    }, error = function(e) {
      return(NULL)
    })
    if(is.null(assigned_to)) { return() }

    # Check to make sure assigned_to is a function, not a value
    if(is_primitive(assigned_to) &&
       deparse(assigned_to) == ".Primitive(\"function\")") {

      # Need to create package cache from scratch
      if(!"internal_fn_defn" %in% names(cache_this)) {
        cache_this[["internal_fn_defn"]] = list()
      }

      # Need to create this function's cache from scratch or add to existing
      # cache
      if(!function_name %in% names(cache_this[["internal_fn_defn"]])) {
        cache_this[["internal_fn_defn"]][[function_name]] = c(variable_name)
      } else {
        cache_this[["internal_fn_defn"]][[function_name]] = c(
          cache_this[["internal_fn_defn"]][[function_name]],
          variable_name)
      }
    }
  }

  # No need to return, we modified the environment
}

#' Determines whether an R object name needs to be backticked for to be used.
#'
#' This is a simple wrapper for two cases where function names need to be
#' backticked -- first, if the name is a reserved keyword, second if the name
#' has certain symbols in its name.
#'
#' @param text Function name to check
#' @return A character vector with the fixed version of the text
name_need_quote = function(text) {
  # If it's in the reserved keywords set, it needs to be backticked.
  if(text %in% c("while", "repeat", "next", "if", "function",
                 "for", "break", "else", "in")) {
    return(paste0("`", text, "`"))
  }

  # If it's got non-standard symbols, it needs to be backticked.
  if(grepl("^[A-Za-z0-9.][A-Za-z0-9._]*$", text)) { return(text) }
  else { return(paste0("`", text, "`")) }

}

#' Confirm which objects from a package namespace are functions
#'
#' This is a simple wrapper for checking if everything in a package namespace
#' is a function or something else.
#'
#' @param package_name The name of the package we're checking; needed to prefix
#' the namespace of the function call.
#' @param candidate_functions Vector of character vectors for function names
#' we're checking.
#' @param public Determines how many colons to put in the
#' @return A vector of booleans for whether or not each item is a function.
#' @importFrom rlang is_function parse_expr
which_are_functions = function(package_name, candidate_functions, public) {
  # Set ::/::: prefix in namespace depending on if it's public or not
  # Then parse_expr to get a call; eval to make the call a function closure
  # is_function to check if it's a function.
  result = vapply(
    candidate_functions,
    function(x) {
      is_function(eval(parse_expr(
          paste0(package_name,
                 ifelse(public,
                        "::",
                        ":::"),
                 name_need_quote(x))
      )))
    },
    TRUE
  )

  return(result)
}

#' Extracts all the functions from a package
#'
#' Loads a package, gets all the functions in the package, cache the result.
#' @param package_name Character vector name of a package
#' @param cache_this an environment -- used to memoise the function.
#' @return Vector of character strings containing all the functions in the
#' package, namedspaced appropriately.
get_functions_from_package = function(package_name, cache_this) {

  # If we've already run this then we should return the cached version.
  # This, aagin, is basically a primitive memoisation implementation.
  if("gffp" %in% names(cache_this) &&
     package_name %in% names(cache_this[["gffp"]])) {
    return(cache_this[["gffp"]][[package_name]])
  }

  # Load the package so we can find this stuff on the search path.
  error_loading = tryCatch({
    suppressMessages(invisible(library(package_name, character.only = TRUE)))
    FALSE
  }, error = function(e) {
    return(TRUE)
  })
  if(error_loading) {
    stop("Error loading package `", package_name, "` while mapping package.")
  }

  # What it exports (public)
  # This .Depends variable shows up in a few packages, I think as part of one
  # of the object systems, and since it doesn't export properly and can't be
  # accessed, let's just drop it from the list of stuff we care about.
  illegal_exports = c(".Depends")

  public_function_vector = ls(paste0("package:", package_name),
                              all.names = TRUE)
  public_function_vector = setdiff(
    public_function_vector,
    illegal_exports)

  # If it's in the ls and it's not public, then it's private.
  private_function_vector = setdiff(
    ls(getNamespace(package_name), all.names = TRUE),
    public_function_vector)

  # Okay, now strip the exports that aren't functions
  if(length(public_function_vector)) {
    public_function_vector = public_function_vector[
      which_are_functions(package_name, public_function_vector, 1)]
  }

  if(length(private_function_vector)) {
    private_function_vector = private_function_vector[
      which_are_functions(package_name, private_function_vector, 0)]
  }

  # Add this to the cache.
  cache_this[["gffp"]][[package_name]] = list(public = public_function_vector,
                                              private = private_function_vector)

  # Return data
  return(cache_this[["gffp"]][[package_name]])
}

#' Which foreign package is a function being loaded from?
#'
#' Figure out which, if any, namespace a function is being loaded from (and
#' if it is private or public) and return the information.
#'
#' @param current_function Name of the function we care about
#' @param package_name Package it is being called from
#' @param cache_this An environment -- used to handle the memoisation
#' @return A character vector containing the namespaced function call or
#' the namespace INVALID for cases where we've failed.
#' @importFrom methods findFunction
where_is_foreign_function_from = function(current_function,
                                          package_name,
                                          cache_this) {
  # If this is cached, skip the function -- this is primitive memoisation
  if("wifff" %in% names(cache_this) &&
     current_function %in% names(cache_this[["wifff"]])) {
    return(cache_this[["wifff"]][[current_function]])
  }

  # The package's namespace is where the package thinks the function is from.
  namespace_package = getNamespace(package_name)
  # Okay, where do we get it from?
  function_stack = findFunction(current_function, where=namespace_package)

  # We basically want to iterate through the namespace options to find the
  # first non-null one.
  if(length(function_stack)) {
    found_package_name = ""
    for(candidate in function_stack) {
      # It appears to be imported. Let's see from where.
      if(grepl("^imports:", environmentName(candidate))) {
        external_environment_name = environmentName(
          environment(
            get0(current_function,
                 envir = namespace_package)))

        if(external_environment_name != "") {
          cache_this[["wifff"]][[current_function]] =
            paste0("package:", external_environment_name)

          return(cache_this[["wifff"]][[current_function]])
        }
        # If we couldn't find where it's imported from, we should
        # move on. I don't think this will happen often.
        next
      }

      # First non-import namespace -- we'll settle for this.
      cache_this[["wifff"]][[current_function]] =
        environmentName(candidate)
      return(cache_this[["wifff"]][[current_function]])
    }
  }

  # We didn't find anything -- how on earth is this function importing?
  cache_this[["wifff"]][[current_function]] = ""
  return(cache_this[["wifff"]][[current_function]])
}
