collect_errors <- function(results) {
  vtg::log$debug("Checking for errors in result.")
  # loop over results
  errors <- c()

  for (i in seq_along(results)) {
    result <- results[[i]]
    if (!is.null(result$error)) {
      vtg::log$info("Error found in the subtasks")
      vtg::log$debug(" - Node '{result$node}'")
      vtg::log$debug(" - Org. '{result$organization}'")
      vtg::log$debug(" - Error: '{result$error}'")
      errors <- c(errors, list(result))
    }
  }
  return(errors)
}