#' Topological graph sort
#'
#' Graph is a `list` which for each node contains a vector of child nodes
#' in the returned list, parents appear before their children.
#'
#' Implementation of `Kahn` algorithm with a modification to maintain the order of input elements.
#'
#' @param graph (`named list`) with node vector elements
#' @keywords internal
topological_sort <- function(graph) {
  # compute in-degrees
  in_degrees <- list()
  for (node in names(graph)) {
    in_degrees[[node]] <- 0
    for (to_edge in graph[[node]]) {
      in_degrees[[to_edge]] <- 0
    }
  }

  for (node in graph) {
    for (to_edge in node) {
      in_degrees[[to_edge]] <- in_degrees[[to_edge]] + 1
    }
  }

  # sort
  visited <- 0
  sorted <- list()
  zero_in <- list()
  for (node in names(in_degrees)) {
    if (in_degrees[[node]] == 0) zero_in <- append(zero_in, node)
  }
  zero_in <- rev(zero_in)

  while (length(zero_in) != 0) {
    visited <- visited + 1
    sorted <- c(zero_in[[1]], sorted)
    for (edge_to in graph[[zero_in[[1]]]]) {
      in_degrees[[edge_to]] <- in_degrees[[edge_to]] - 1
      if (in_degrees[[edge_to]] == 0) {
        zero_in <- append(zero_in, edge_to, 1)
      }
    }
    zero_in[[1]] <- NULL
  }

  if (visited != length(in_degrees)) {
    stop(
      "Graph is not a directed acyclic graph. Cycles involving nodes: ",
      paste0(setdiff(names(in_degrees), sorted), collapse = " ")
    )
  } else {
    return(sorted)
  }
}

#' Checks whether a graph is a `Directed Acyclic Graph (DAG)`
#'
#' @inheritParams topological_sort
#' @return `logical(1)` `TRUE` if the graph is a `DAG`; `FALSE` otherwise
#' @keywords internal
is_dag <- function(graph) {
  inherits(try(topological_sort(graph), silent = TRUE), "try-error")
}
