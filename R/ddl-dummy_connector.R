#' @export
open_dummy_connection <- function(username, password) {
  if (password != "password") {
    stop("Invalid credentials")
  }
  conn <- TRUE
  return(conn)
}

#' @export
close_conn <- function(conn) {
  return(NULL)
}


#' @export
# getting data function e.g. rice_read (provided for app developer)
get_dummy_data <- function(conn) {
  if (!conn) {
    stop("Invalid connection object")
  }
  return(data.frame(id = 1:10, val = 1:10))
}