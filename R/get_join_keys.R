#' Function to get join keys from a `` object
#' @param data `` - object to extract the join keys
#' @return Either `JoinKeys` object or `NULL` if no join keys
#' @export
get_join_keys <- function(data) {
  UseMethod("get_join_keys", data)
}

#' @rdname get_join_keys
#' @export
get_join_keys.default <- function(data) {
  stop("get_join_keys function not implemented for object of class ", toString(class(data)))
}

#' @rdname get_join_keys
#' @export
get_join_keys.teal_data <- function(data) {
  data@join_keys
}

#' @rdname get_join_keys
#' @export
get_join_keys.JoinKeys <- function(data) {
  data
}

#' @rdname get_join_keys
#' @export
get_join_keys.TealData <- function(data) {
  data$get_join_keys()
}

#' @rdname get_join_keys
#' @inheritParams mutate_join_keys
#' @param value value to assign
#' @export
`get_join_keys<-` <- function(data, dataset_1, dataset_2 = NULL, value) {
  UseMethod("get_join_keys<-", data)
}


#' @rdname get_join_keys
#' @inheritParams mutate_join_keys
#' @export
`get_join_keys<-.JoinKeys` <- function(data, dataset_1, dataset_2 = NULL, value) {
  # The reason this passthrough method is defined is to prevent a warning message
  # The assignment is performed by `get_join_keys.JoinKeys` and `[<-.JoinKeys` combination
  #  as well as `JoinKeys` being an R6 class
  data
}

#' @rdname get_join_keys
#' @export
`get_join_keys<-.teal_data` <- function(data, dataset_1, dataset_2 = NULL, value) {
  # The reason this passthrough method is defined is to prevent a warning message
  # The assignment is performed by `get_join_keys.teal_data` and `[<-.JoinKeys` combination
  #  as well as `JoinKeys` being an R6 class
  data
}
