collect_errors <- function(results) {
  vtg::log$debug("Checking for errors in result.")
  # loop over results
  errors <- c()

  for (i in seq_along(results)) {
    result <- results[[i]]
    if (!is.list(result) && exists("error", result)) {
      vtg::log$info("Error found in the subtasks")
      vtg::log$debug(" - Node '{result$node}'")
      vtg::log$debug(" - Org. '{result$organization}'")
      vtg::log$debug(" - Error: '{result$error}'")
      errors <- c(errors, list(result))
    }
  }

  if (length(errors) == 0) {
    vtg::log$debug("No errors found.")
    return(NULL)
  }

  report <- list(
    error = glue::glue("{length(errors)} node(s) reported error(s)."),
    errors = errors
  )
  return(report)
}