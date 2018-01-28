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

#' Gets function calls of interest in a "line" of R code.
#'
#' This function takes a line of R code (which should be of type `language`) and
#' recursively processes the statements within that line, extracting those that
#' are function calls. We exempt the built-in packages (except the package being
#' evaluated, if it is a built-in package). Functions that are not found are
#' namespaced with the "INVALID::" prefix.
#'
#' @param line A line of R code (type `language`)
#' @param all_functions A vector of character strings listing every function in
#' the package; used to check the current namespace
#' @param package_name A character vector containing the name of the package
#' @param function_name A character vector containing the name of the function
#' being called.
#' @param argument_names A vector of character strings containing the arguments
#' of the current function; this is in case a function is passed through in the
#' argument list.
#' @param cache_this An environment, used to memoise the calls and keep track of
#' functions defined inside this line.
#' @return NULL or a vector of character strings containing the namespaced names
#' of function calls.
#' @importFrom rlang lang_tail lang_head lang_name is_lang
get_calls_from_line = function(line,
                               all_functions,
                               package_name,
                               function_name,
                               argument_names,
                               cache_this) {

  # If the line processed isn't a language call, then it's a symbol
  # (useless for us) or NULL (also useless for us)
  if(is_lang(line)) {

    # The current function -- will not return package prefix.
    current_function = tryCatch({
      ln = lang_name(line)

      # In case the current call is a function definition, we need to know for
      # later calls
      capture_internal_function_def(line, function_name, cache_this)

      ln
      }, error = function(e) {
        # Certain cases might be TRUE for is_lang but FALSE when you run
        # lang_name; one-term formula objects are one such. But that's OK,
        # these aren't functions, so we can ignore them.
        return(NULL)
    })
    if(is.null(current_function)) return(NULL)

    # Special handling for .Internal / .Primitive, mostly applies for
    # the `base` package. Basically, functions inside these wrappers get
    # non-standardly evaluated; sometimes people pass as strings, otherwise as
    # code calls. I want to cut off the recursion rather than process these.
    if(current_function %in% c(".Internal", ".Primitive"))
    {
      return(list(paste0("base::", current_function)))
    }

    # Recursively check for other functions from the line.
    recursive_descent = unlist(lapply(lang_tail(line),
                                      get_calls_from_line,
                                      all_functions,
                                      package_name,
                                      function_name,
                                      argument_names,
                                      cache_this),
                               recursive=TRUE)

    # Okay, so if we're here, we have a function name.
    # There are three possible places we're going to find this function;
    # First: In a foreign package, imported into current package
    # Second: In the current package namespace
    # Third: Implicit import by :: or ::: prefix.

    # First, let's check to see if we can find it at all.
    found_package_name = where_is_foreign_function_from(current_function,
                                                        package_name,
                                                        cache_this)

    if(is.null(found_package_name) || found_package_name == "") {
      # If we can't find it, then it's either in the current package or nowhere.
      if(current_function %in% all_functions) {
        found_package_name = ""
      } else {
        found_package_name = NA
      }
    }


    # We only care if it's not one of these packages -- unless we're trying
    # to explore that package -- also exempt the R6 methods.
    base_packages = c("base", "package:base", "package:compiler",
                      "package:datasets", "package:graphics",
                      "package:grDevices", "package:grid", "package:methods",
                      "package:parallel", "package:splines", "package:stats",
                      "package:stats4", "package:tcltk", "package:tools",
                      "package:translations", "package:utils",
                      "package:R6_capsule",
                      "package:tools:rstudio", "tools:rstudio")
    base_packages = setdiff(base_packages, c(package_name,
                                             paste0("package:", package_name)))

    # If it's a base function, then ignore whatever we found at this node, just
    # get the recursive functions
    if(found_package_name %in% base_packages) {
      return(recursive_descent)
    }

    if(!is.na(found_package_name) && length(found_package_name) &&
       !found_package_name %in% c("", paste0("package:", package_name))) {

      # We've got a function for a foreign package. Is it public or private?
      guess = gsub("package:", "", found_package_name)
      foreign = get_functions_from_package(guess, cache_this)

      # Remove the package: prefix; concatenate with two or three colons;
      # and then concatenate the function.
      current_function = paste0(guess,
                                ifelse(current_function %in% foreign$public,
                                       "::",
                                       ":::"),
                                current_function)
    } else if(is.na(found_package_name) || found_package_name == "") {
      # We have no function from a foreign namespace. Let's introspect some more
      # about the current instruction to figure out if there was a namespace
      # prefix we could make use of.
      current_function = tryCatch({
        paste0(as.character(lang_tail(lang_head(line))[[1]]),
               as.character(lang_head(lang_head(line))),
               current_function)},
        error = function(e) {
          # If the function name matches an argument in the parent function's
          # formals, then return NULL; clearly it's a passthrough wrapper for
          # a function that we can't possibly know.
          if(current_function %in% argument_names) {
            return(NULL)
          }

          # If the function name matches a variable to which a function was
          # assigned within the current function, it's that.
          if(current_function %in%
             cache_this[["internal_fn_defn"]][[function_name]]) {
            return(NULL)
          }

          # This is bad and means we couldn't figure out where the heck
          # the function was from -- maybe look at warning or even erroring
          # on this later.
          return(paste0("INVALID::", current_function))
        })
    }

    # If we're good, then let's return what we've got and the recursive stuff.
    return(list(current_function, recursive_descent))
  }

  # It wasn't a language call, it was a symbol, so we're at a dead end.
  return(NULL)
}

#' Gets all calls from a function
#'
#' The core workhorse of werner, this function gets all calls made from a given
#' function in a package by breaking the function call into a series of
#' discrete lines and handing each line off to `get_calls_from_line`.
#'
#' @param function_name Character vector name of a function, namespaced
#' @param all_functions Vector of character strings listing all functions in
#' the package's namespace
#' @param package_name Name of the package we're searching
#' @param cache_this Environent to hold the progress bar and memoised progress
#' @returns List of vectors of character strings describing each function's
#' calls.
#' @importFrom rlang lang_args parse_expr is_function fn_fmls_names
# Given a function, split it by line and get the calls on each line
get_calls_from_function = function(function_name,
                                   all_functions,
                                   package_name,
                                   cache_this) {

  # Tick the progress bar.
  tick_progress_bar(cache_this, length(all_functions))

  # First, let's get whatever this thing is.
  # parse_expr changes character string to an unquoted r function call
  # eval does the call and returns a namespaced, environmented function.
  candidate_function = eval(parse_expr(function_name))

  # This error should never trigger since we previously checked if these
  # objects were functions, but you never know.
  if(!is_function(candidate_function)) {
    return(NULL)
  }

  # rlang fn_fmls_names extracts argument names; a handful of the reserve
  # keywords might fail on this including base::`break`, so we return NULL
  # for those.
  argument_names = tryCatch({
    setdiff(fn_fmls_names(candidate_function), c("..."))
  }, error = function(e) {
    return(NULL)
  })

  # body gets function body
  function_body = body(candidate_function)

  # Occasionally this happens when the function just straight returns an
  # object; Zelig::summary.Arima is one example of something that triggered
  # this.
  if(!is_lang(function_body)) {
    return(NULL)
  }

  # Split body into list of lines, each to be processed.
  each_line = lang_args(function_body)

  # Now that we have a list, lapply one line at a time.
  results = unique(unlist(lapply(each_line,
                                 get_calls_from_line,
                                 all_functions,
                                 package_name,
                                 function_name,
                                 argument_names,
                                 cache_this)))

  # If we have any INVALID calls and some functions that were defined inside the
  # function, then it's possible some of the invalid calls are to those
  # functions. If so, let's remove them from the list.
  if(any(grepl("^INVALID::", results)) &&
     length(cache_this[["internal_fn_defn"]][[function_name]])) {

    # Invalid calls
    invalid_ids = which(grepl("^INVALID::", results))
    # Calls that match the internal functions.
    found_funcs = which(gsub("^INVALID::", "", results) %in%
                          cache_this[["internal_fn_defn"]][[function_name]])

    # Drop the intersection
    afflicted_ids = intersect(invalid_ids, found_funcs)
    results = results[-afflicted_ids]
  }

  return(results)
}
