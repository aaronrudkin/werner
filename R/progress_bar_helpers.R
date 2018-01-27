# Clear a progress bar
clear_progress_bar = function(cache_this) {
  # If there's no progress bar, then we don't need to clear it.
  if(!cache_this[["show_progress"]]) {
    return()
  }

  # Manually close the progress bar.
  if("_progress_bar" %in% names(cache_this)) {
    close(cache_this[["_progress_bar"]])
  }
}

# Setup or tick a progress bar.
#' @importFrom utils txtProgressBar setTxtProgressBar
tick_progress_bar = function(cache_this, max_length) {
  # If we don't want a progress bar, dummy out of this function
  if(!cache_this[["show_progress"]]) {
    return()
  }

  if(!"_progress" %in% names(cache_this)) {
    # Initialize to 0/max_length
    cache_this[["_progress_bar"]] = txtProgressBar(min = 0,
                                                   max = max_length,
                                                   style = 3)
    cache_this[["_progress"]] = 0
  }
  # Increment and update.
  cache_this[["_progress"]] = cache_this[["_progress"]] + 1
  setTxtProgressBar(cache_this[["_progress_bar"]],
                    cache_this[["_progress"]])
}
