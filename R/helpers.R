#' @title Standard Error Message
#' @description
#' This function creates a standard error message that can be used by the algorithms
#' to report errors in a standard way. It returns a list with the following fields:
#' - error: the error message
#' - node: the node id
#' - organization: the organization id
#' @param error The error message
#' @author F.C. Martin
#' @export
error_format <- function(error)  {
  identity <- get_identity()
  return(list(
    error = error,
    node = identity$node_id,
    organization = identity$organization_id
  ))
}

#' @title Get Node ID from Token
#' @description
#' This function reads the token from the TOKEN_FILE environment variable and
#' extracts the node id from it.
#' @author F.C. Martin
#' @export
get_node_id <- function() {
  identity <- get_identity()
  return(identity$node_id)
}

#' @title Get Organization ID from Token
#' @description
#' This function reads the token from the TOKEN_FILE environment variable and
#' extracts the organization id from it.
#' @author F.C. Martin
#' @export
get_organization_id <- function() {
  identity <- get_identity()
  return(identity$organization_id)
}


#' @title Get Identity from Token
#' @description
#' This function reads the token from the TOKEN_FILE environment variable and
#' extracts the identity from it. It returns a list with the following fields:
#' - node_id: the node id
#' - organization_id: the organization id
#' - image: the image name
#' - task_id: the task id
#' @author F.C. Martin
#' @export
get_identity <- function() {
  # Read the contents of file input.txt into 'input_data'
  token_file <- Sys.getenv("TOKEN_FILE", unset = NA)

  # In case we the user is mocking, we don't have a token file
  dummy <- list(node_id = -1, organization_id = -1, image = "mock", task_id = -1)
  if (is.na(token_file)) {
    vtg::log$warn("TOKEN_FILE environment variable not set. Are you mocking?das das")
    return(dummy)
  }

  vtg::log$debug("Loading token from '{token_file}'")
  token <- readChar(token_file, file.info(token_file)$size)

  strings <- unlist(strsplit(token, ".", fixed = TRUE))
  json_string <- rawToChar(base64enc::base64decode(strings[2]))

  jwt_token <- rjson::fromJSON(json_string)
  if ("sub" %in% names(jwt_token)) {
    identity <- jwt_token$sub
  } else if ("identity" %in% names(jwt_token)) {
    identity <- jwt_token$identity
  } else {
    vtg::log$error("Could not find 'sub' or 'identity' in token.")
    return(dummy)
  }
  vtg::log$debug("Identity is '{identity}'")
  return(identity)
}