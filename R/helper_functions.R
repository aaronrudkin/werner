# Clear a progress bar
clear_progress_bar = function(cache_this) {
  use_good = requireNamespace("progress", quietly = TRUE)

  if(!cache_this[["show_progress"]]) {
    return()
  }

  if(use_good) {
    # progress pbs auto-clear.
  } else {
    if("_progress_bar" %in% names(cache_this)) {
      close(cache_this[["_progress_bar"]])
    }
  }
}

# Setup or tick a progress bar.
#' @importFrom utils txtProgressBar setTxtProgressBar
tick_progress_bar = function(cache_this, max_length) {
  use_good = requireNamespace("progress", quietly = TRUE)

  if(!cache_this[["show_progress"]]) {
    return()
  }

  if(use_good) {
    if(!"_progress_bar" %in% names(cache_this)) {
      cache_this[["_progress_bar"]] =
        progress::progress_bar$new(total = max_length)
    }
    cache_this[["_progress_bar"]]$tick()
  } else {
    if(!"_progress" %in% names(cache_this)) {
      cache_this[["_progress_bar"]] = txtProgressBar(min = 0,
                                                     max = max_length,
                                                     style = 3)
      cache_this[["_progress"]] = 0
    }
    cache_this[["_progress"]] = cache_this[["_progress"]] + 1
    setTxtProgressBar(cache_this[["_progress_bar"]],
                      cache_this[["_progress"]])
  }
}


# Determines whether an R object needs to be backticked.
name_need_quote = function(text) {
  if(grepl("^[A-Za-z0-9.][A-Za-z0-9._]*$", text)) { return(text) }
  else { return(paste0("`", text, "`")) }
}

#' @importFrom rlang is_function parse_expr
which_are_functions = function(package_name, candidate_functions, public) {
  # Just try to clobber it as an expression with the package name prefix.
  result = sapply(candidate_functions, function(x) {
    is_function(
      eval(
        parse_expr(
          paste0(package_name,
                 ifelse(public,
                        "::",
                        ":::"),
                 name_need_quote(x))
        )
      )
    )
  })

  return(result)
}

# Given a package name, figure out which of its methods are public and which
# are private
get_functions_from_package = function(package_name, cache_this) {
  if("gffp" %in% names(cache_this) &
     package_name %in% names(cache_this[["gffp"]])) {
    return(cache_this[["gffp"]][[package_name]])
  }

  # Load the package so we can find this stuff on the search path.
  suppressMessages(invisible(library(package_name, character.only = TRUE)))

  # What it exports (public)
  illegal_exports = c(".Depends")

  public_function_vector = ls(paste0("package:", package_name), all.names = TRUE)
  public_function_vector = setdiff(
    public_function_vector,
    illegal_exports)
  # Everything in the namespace (private + public), setdiffed on the public stuff.
  private_function_vector = setdiff(
    ls(getNamespace(package_name), all.names = TRUE),
    public_function_vector)

  # Knock out the non-function variables.
  public_function_vector = public_function_vector[
    which_are_functions(package_name, public_function_vector, 1)]

  private_function_vector = private_function_vector[
    which_are_functions(package_name, private_function_vector, 0)]

  cache_this[["gffp"]][[package_name]] = list(public = public_function_vector,
                                              private = private_function_vector)

  # Return data
  return(list(public = public_function_vector,
              private = private_function_vector))
}

# Try a few different methods to figure out where the function is from.
where_is_foreign_function_from = function(current_function,
                                          package_name,
                                          cache_this) {
  if("wifff" %in% names(cache_this) &
     current_function %in% names(cache_this[["wifff"]])) {
    return(cache_this[["wifff"]][[current_function]])
  }

  namespace_package = getNamespace(package_name)
  function_stack = findFunction(current_function, where=namespace_package)

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
          return(paste0("package:", external_environment_name))
        }
        next
      }

      cache_this[["wifff"]][[current_function]] =
        environmentName(candidate)
      return(environmentName(candidate))
    }
  }

  cache_this[["wifff"]][[current_function]] = ""
  return("")
}

#' @importFrom rlang lang_tail lang_head lang_name is_lang
#' @importFrom methods findFunction
# Given a line, either recursive break it apart into symbols or
# if you get to symbols, return the symbols
get_calls_from_line = function(line,
                               all_functions,
                               package_name,
                               cache_this) {

    # If the line processed isn't a language call, then it's a symbol
  # (useless for us) or NULL (also useless for us)
  if(is_lang(line)) {
    # The current function -- will not return package prefix.
    current_function = tryCatch({
      lang_name(line)
      }, error = function(e) {
        # Certain cases might be TRUE for is_lang but FALSE when you run
        # lang_name; one-term formula objects are one such. But that's OK,
        # these aren't functions, so we can ignore them.
        return(NULL)
    })
    if(is.null(current_function)) return(NULL)

    # Recursively check for other functions from the line.
    recursive_descent = unlist(lapply(lang_tail(line),
                                      get_calls_from_line,
                                      all_functions,
                                      package_name,
                                      cache_this),
                               recursive=TRUE)

    # If we didn't find a function at all -- this typically means there's an
    # anonymous function constructed that we can't resolve -- then just
    # return the recursive results
    if(is.null(current_function)) {
      return(recursive_descent)
    }

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

    # We only care if it's not one of these packages.
    base_packages = c("base", "package:base", "package:compiler",
                      "package:datasets", "package:graphics", "package:grDevices",
                      "package:grid", "package:methods", "package:parallel",
                      "package:splines", "package:stats", "package:stats4",
                      "package:tcltk", "package:tools", "package:translations",
                      "package:utils")

    # If it's a base function, then ignore whatever we found at this node, just
    # get the recursive functions
    if(found_package_name %in% base_packages) {
      return(recursive_descent)
    }

    if(!is.na(found_package_name) & length(found_package_name) &
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
          return(paste0("INVALID::", current_function))
        })
    }

    # Okay, now return this and the stuff we already had
    return(list(current_function, recursive_descent))
  }

  return(NULL)
}

#' @importFrom rlang lang_args parse_expr is_function
# Given a function, split it by line and get the calls on each line
get_calls_from_function = function(function_name,
                                   all_functions,
                                   package_name,
                                   cache_this) {

    # Tick the progress bar.
  tick_progress_bar(cache_this, length(all_functions))

  # First, let's get whatever this thing is.
  candidate_function = eval(parse_expr(function_name))

  # This error should never trigger since we previously checked if these
  # objects were functions, but you never know.
  if(!is_function(candidate_function)) {
    return(NULL)
  }

  # parse_expr changes character string to an unquoted r function call
  # eval does the call and returns a namespaced, environmented function.
  # body gets function body
  function_body = body(candidate_function)

  # Occasionally this happens when the function just straight returns an
  # object; Zelig::summary.Arima is one example of something that triggered
  # this.
  if(!is_lang(function_body)) {
    return(NULL)
  }

  each_line = lang_args(function_body)

  # Now that we have a list, lapply one line at a time.
  return(unique(unlist(lapply(each_line,
                              get_calls_from_line,
                              all_functions,
                              package_name,
                              cache_this))))
}
