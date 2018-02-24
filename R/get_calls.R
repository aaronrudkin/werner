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

  } else if(is.symbol(line) && as.character(line) %in% all_functions) {
    # Try to evaluate the symbol somewhere in a search namespace
    provisional_find = getAnywhere(as.character(line))

    # If we don't find it, it's a null
    if(!length(provisional_find$where)) {
      return(NULL)
    }

    chunk_package_name = strsplit(provisional_find$where[1], ":")
    return(paste0(chunk_package_name[[1]][length(chunk_package_name[[1]])],
                  ifelse(any(provisional_find$visible == TRUE),
                         "::",
                         ":::"),
                  provisional_find$name))
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
