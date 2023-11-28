# get_code_dependency ----

#' Return the lines of code (with side-effects) needed to reproduce the object
#'
#' @details This function assumes that object relationships are established using the `<-`, `=`, or `->` assignment
#' operators. It does not support other object creation methods like `assign` or `<<-`, nor non-standard-evaluation
#' methods. To specify relationships between side-effects and objects, you can use the comment tag
#' `# @linksto object_name` at the end of a line where the side-effect occurs.
#'
#' @param code An `expression` with `srcref` attribute or a `character` with the code.
#' @param names A `character(n)` with object names.
#'
#' @return `character` vector of elements of `code` calls that were required to build the side-effects and
#' influencing objects having and impact on the `object`
#'
#' @keywords internal
get_code_dependency <- function(code, names) {
  assert_classes(code, names)

  if (is_empty(code)) return(code)

  code <- parse(code, keep.source = TRUE) 
  pd <- utils::getParseData(code)
  assert_names(names, pd)

  calls_pd <- extract_calls(pd)

  graph <- code_graph(calls_pd)
  indexes <- unlist(lapply(names, function(x) graph_parser(x, graph)))
  as.character(code[unique(indexes)])
}

#' Split the result of `utils::getParseData()` into separate calls
#' @param pd (`data.frame`) A result of `utils::getParseData()`.
#'
#' @return A `list` of length equal to the number of calls in code used to create `pd`. Each element of the returned
#' `list` is one call of a structure similar to `pd` (`utils::getParseData()`) but limited to three columns
#' (`"token"`, `"text"`, `"id"`).
#' @keywords internal
#' @noRd
extract_calls <- function(pd) {
  get_children <- function(pd, parent) {
    idx_children <- abs(pd$parent) == parent
    children <- pd[idx_children, c("token", "text", "id")]
    if (nrow(children) == 0) {
      return(NULL)
    }

    if (parent > 0) {
      do.call(rbind, c(list(children), lapply(children$id, get_children, pd = pd)))
    }
  }
  calls <- lapply(pd[pd$parent == 0, "id"], get_children, pd = pd)
  fix_comments(calls)
}

fix_comments <- function(calls) {
  # If the first token is a COMMENT, then it belongs to the previous call.
  if (length(calls) >= 2) {
    for(i in 2:length(calls)){
      if (grepl("@linksto", calls[[i]][1, "text"])) {
        calls[[i-1]] <- rbind(calls[[i-1]], calls[[i]][ 1, ])
        calls[[i  ]] <-                     calls[[i]][-1, ]
      }
    }
  }
  calls
}

# code_graph ----

#' Create Object Dependencies Graph Within Parsed Code
#'
#' @description This function constructs a dependency graph that identifies the relationships between objects in
#' parsed code. It helps you understand which objects are needed to recreate a specific object.
#'
#' @param calls_pd A `list` of `data.frame`s, which is a result of `utils::getParseData()` grouped into separate calls.
#' A result of `extract_calls()` function.
#'
#' @return A `list` (of length of input `calls_pd`) where each element represents one call. Each element consists of a
#' character vector containing names of objects that were influenced by this call, and names of objects influencing this
#' call. Influencers appear after `":"` string, e.g. `c("a", ":", "b", "c")` means a call influenced object named `a`,
#' and the object was influenced by objects named `b` and `c`. If an object was marked with `@linksto` side effects tag
#' then it appears as an influenced object at the beginning of a vector.
#'
#' @keywords internal
#' @noRd
code_graph <- function(calls_pd) {

  cooccurence <- extract_occurrence(calls_pd)

  side_effects <- extract_side_effects(calls_pd)

  graph <- mapply(c, side_effects, cooccurence, SIMPLIFY = FALSE)

  remove_graph_duplicates(graph)

}

#' Extract Object Occurrence
#'
#' @description Extract objects occurrence within calls passed by `calls_pd`. It also detects which objects are
#' influenced by others within a call, and which are influencers.
#'
#' @param calls_pd A `list` of `data.frame`s, which is a result of `utils::getParseData()` grouped into separate calls.
#' A result of `extract_calls()` function.
#' @return A `list` (of length of input `calls_pd`) where each element represents one call. Each element consists of a
#' character vector containing names of objects that were influenced by this call, and names of objects influencing this
#' call. Influencers appear after `":"` string, e.g. `c("a", ":", "b", "c")` means a call influenced object named `a`,
#' and the object was influenced by objects named `b` and `c`.
#' @keywords internal
#' @noRd
extract_occurrence <- function(calls_pd) {

  used_in_function <- function(call, rows) {

    # If an object is a function parameter,
    # then in calls_pd there is a `SYMBOL_FORMALS` entry for that object.
    objects <- sapply(rows, function(row) call$text[row])

    vapply(objects, function(object) {
      if (any(call[call$token == "SYMBOL_FORMALS", "text"] == object) && any(call$token == "FUNCTION")) {
        object_sf_ids <- call[call$text == object & call$token == "SYMBOL", "id"]
        function_start_id <- call[call$token == "FUNCTION", "id"]
        all(object_sf_ids > function_start_id)
      } else {
        FALSE
      }
    },
      logical(1)
    )
  }

  extract_functions <- function(calls_pd) {
    # Detect functions created within code.
    pd <- do.call(rbind, calls_pd)
    symbols <- pd[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL"), c('token', 'text')]
    symbols_table <- table(unique(symbols)$text)
    names(symbols_table[symbols_table == 2])
  }

  functions <- extract_functions(calls_pd)

  lapply(
    calls_pd,
    function(x) {
      sym_cond <- which(x$token == "SYMBOL" | (x$token == "SYMBOL_FUNCTION_CALL" & x$text %in% functions))
      sym_cond <- sym_cond[!used_in_function(x, sym_cond)]

      # Watch out for SYMBOLS after $ and @. For x$a x@a: x is object, a is not.
      # For x$a, a's ID is $'s ID-2 so we need to remove all IDs that have ID = $ID - 2.
      dollar_ids <- x[x$token %in% c("'$'", "'@'"), "id"]
      if (length(dollar_ids)) {
        object_ids <- x[sym_cond, "id"]
        after_dollar <- object_ids[(object_ids - 2) %in% dollar_ids]
        sym_cond <- setdiff(sym_cond, which(x$id %in% after_dollar))
      }

      # If there was an assignment operation detect direction of it.
      if (length(sym_cond) >= 2) {
        ass_cond <- grep("ASSIGN", x$token)
        if (length(ass_cond)) { # NOTE(1)
          sym_cond <- sym_cond[sym_cond > ass_cond] # NOTE(2)
          text <- x[sort(c(sym_cond, ass_cond)), "text"]

          ans <- if (text[1] == "->") { # NOTE(3)
            rev(text[-1])
          } else {
            text[-1]
          }
          c(ans[1], ":", unique(ans[-1]))
        } else {
          x[sym_cond, "text"]
        }
      } else {
        x[sym_cond, "text"]
      }

      ### NOTE(3): What if there are 2+ assignments, e.g. a <- b -> c or e.g. a <- b <- c.
      ### NOTE(2): For cases like 'eval(expression(b <- b + 2))' removes 'eval(expression('.
      ### NOTE(1): Cases like 'data(iris)' that do not have an assignment operator.
    }
  )
}

#' Extract Side Effects
#'
#' @description Extract all object names from the code that are marked with `@linksto` tag.
#'
#' @details The code sometimes consists of functions calls that create side effects. Those are actions that modify the
#' environment outside of the function environment. Sometimes there is no way to understand, just based on static code
#' analysis, which objects are modified or created by a function call that has side effects. To allow to point to
#' objects that are modified or created by function calls with side effects, a `@linksto` comment tag was introduced.
#' This tag enables to restore the reproducible code for an object, with all side effects that do not directly change
#' this object. Read more about side effects and the usage of `@linksto` tag in [`get_code_dependencies()`] function.
#'
#' @param calls_pd A `list` of `data.frame`s, which is a result of `utils::getParseData()` grouped into separate calls.
#' A result of `extract_calls()` function.
#' @return A `list` of length equal to the lenght of `calls_pd` of character vectors of names of objects that are
#' influenced by `@linksto` tag in a corresponding `call` element of `calls_pd`.
#' @keywords internal
#' @noRd
extract_side_effects <- function(calls_pd) {
  lapply(
    calls_pd,
    function(x) {
      linksto <- grep("@linksto", x[x$token == "COMMENT", "text"], value = TRUE)
      unlist(strsplit(sub("\\s*#\\s*@linksto\\s+", "", linksto), "\\s+"))
    }
  )
}

#' @keywords internal
#' @noRd
remove_graph_duplicates <- function(graph) {
  # This function is just a nice to have.
  # This is not crucial for the implementation.
  # Purpose: remove duplicates before ":" and after, in each element of list.

  lapply(
    lapply(graph, paste, collapse = "***"),
    function(x) {
      split <- strsplit(x, split = ":", fixed = TRUE)[[1]]
      deps <- strsplit(split[1], split = "***", fixed = TRUE)[[1]]

      if (length(split) == 1) {
        unique(deps)
      } else if (length(split) == 2) {
        infl <- strsplit(split[2], split = "***", fixed = TRUE)[[1]][-1]
        c(unique(deps), ":", unique(infl))
      }
    }
  )

}

# graph_parser ----

#' Return the indexes of calls of code needed to reproduce the object
#'
#' @param x The name of the object to return code for.
#' @param graph A result of `code_graph()`.
#' @param skip `NULL` or `character` vector. In a recursive call, it is needed to drop parent object to omit dependency
#' cycles.
#'
#' @return `numeric` vector indicating which calls of `graph` are required to build the object passed by name in `x`.
#'
#' @keywords internal
#' @noRd
graph_parser <- function(x, graph, skip = NULL) {

  skip <- c(x, skip)

  occurence <-
    vapply(graph, function(call) {
      if (":" %in% call) {
        call <- call[1:(which(":" == call)-1)]
      }
      x %in% call
    },
    logical(1)
  )

  influencers <-
    unlist(
      lapply(graph[occurence], function(call) {
        if (":" %in% call) {
          call[(which(":" == call)+1):length(call)]
        }
      })
    )
  influencers <- setdiff(influencers, skip)

  if (length(influencers)) {
    influencers_ids <-
      lapply(influencers, function(influencer){
        graph_parser(influencer, graph[1:max(which(occurence))], skip)
      })

    sort(unique(c(which(occurence), unlist(influencers_ids))))
  } else {
    which(occurence)
  }

}

# utils -----------------------------------------------------------------------------------------------------------

#' @keywords internal
#' @noMd
assert_classes <- function(code, names) {
  checkmate::assert_class(code, classes = "character")
  checkmate::assert_character(names)
}

#' @keywords internal
#' @noMd
assert_code <- function(code) {
  if (is.expression(code)) {
    if (!is.null(attr(code, "srcref"))) {
      parsed_code <- code
    } else {
      stop("The 'expression' code input does not contain 'srcref' attribute.")
    }
  }

  if (is.character(code)) {
    parsed_code <- parse(text = code, keep.source = TRUE)
  }
  parsed_code
}

#' @keywords internal
#' @noMd
assert_names <- function(names, pd) {
  symbols <- unique(pd[pd$token == "SYMBOL", "text"])
  if (!all(names %in% symbols)) {
    warning(
      "Object(s) not found in code: ",
      toString(setdiff(names, symbols))
    )
  }
}

#' @keywords internal
#' @noMd
is_empty <- function(code){
  identical(code, character(0)) || identical(trimws(code), "")
}

# # # examples and test -----------------------------------------------------------------------------------------------
#
# code <- "
#
#   a <- 5
#   b <- a + 3
#   a <- a + 6
#   5 -> c # @linksto a
#
# "
#
# graph_expected <- list(
#   c("a"),
#   c("b", ":", "a"),
#   c("a", ":", "a"),
#   c("a", "c")
# )
#
# code_a_expected <- c("a <- 5", "a <- a + 6", "c <- 5")
# code_b_expected <- c("a <- 5", "b <- a + 3")
# code_c_expected <- c("c <- 5")
#
#
# code_2 <- "
#
#   a <- 1
#   b <- identity(x = a)
#   a <- 2
#
# "
#
# graph_expected_2 <- list(
#   c("a"),
#   c("b", ":", "a"),
#   c("a")
# )
#
# code_b_expected_2 <- c("a <- 1", "b <- identity(x = a)")
#
# code_3 <- "
#
#   a <- 1
#   assign('b', 5) # @linksto b
#   b <- b + 2
#
# "
#
# graph_expected_3 <- list(
#   c("a"),
#   c("b"),
#   c("b", ":", "b")
# )
#
# code_b_expected_3 <- c("assign(\"b\", 5)", "b <- b + 2")
#
# code_4 <- "
#   iris[1:5, ] -> iris2
#   iris_head <- head(iris) # @linksto iris3
#   iris3 <- iris_head[1, ] # @linksto iris2
#   classes <- lapply(iris2, class)
# "
#
# graph_expected_4 <- list(
#   c("iris2", ":", "iris"),
#   c("iris3", "iris_head", ":", "iris"),
#   c("iris2", "iris3", ":", "iris_head"),
#   c("classes", ":", "iris2", "class")
# )
#
# code_c_expected_4 <- c(
#   "iris2 <- iris[1:5, ]",
#   "iris_head <- head(iris)",
#   "iris3 <- iris_head[1, ]",
#   "classes <- lapply(iris2, class)"
# )
#
# make_graph <- function(code) {
#   pd <- utils::getParseData(code)
#   calls_pd <- extract_calls(pd)
#   code_graph(calls_pd)
# }
#
# code <- assert_code(code)
# graph <- make_graph(code)
#
# code_2 <- assert_code(code_2)
# graph_2 <- make_graph(code_2)
#
# code_3 <- assert_code(code_3)
# graph_3 <- make_graph(code_3)
#
# code_4 <- assert_code(code_4)
# graph_4 <- make_graph(code_4)
#
# testthat::test_that("code_graph returns proper structure of the dependency graph", {
#
#   testthat::expect_identical(
#     graph,
#     graph_expected
#   )
#
#   testthat::expect_identical(
#     graph_2,
#     graph_expected_2
#   )
#
#   testthat::expect_identical(
#     graph_3,
#     graph_expected_3
#   )
#
#   testthat::expect_identical(
#     graph_4,
#     graph_expected_4
#   )
#
# })
#
# testthat::test_that("graph_parser returns proper code based on code_graph", {
#
#   names <- 'a'
#   indexes <- unlist(lapply(names, function(x) graph_parser(x, graph)))
#   testthat::expect_identical(
#     as.character(code[indexes]),
#     code_a_expected
#   )
#
#   names <- 'b'
#   indexes <- unlist(lapply(names, function(x) graph_parser(x, graph_2)))
#   testthat::expect_identical(
#     as.character(code_2[indexes]),
#     code_b_expected_2
#   )
#
#   names <- 'b'
#   indexes <- unlist(lapply(names, function(x) graph_parser(x, graph_3)))
#   testthat::expect_identical(
#     as.character(code_3[indexes]),
#     code_b_expected_3
#   )
#
#   names <- 'classes'
#   indexes <- unlist(lapply(names, function(x) graph_parser(x, graph_4)))
#   testthat::expect_identical(
#     as.character(code_4[indexes]),
#     code_c_expected_4
#   )
#
# })
#
# testthat::test_that("get_code_dependency returns proper code", {
#
#   testthat::expect_identical(
#     get_code_dependency(code, names = 'a'),
#     code_a_expected
#   )
#
#   testthat::expect_identical(
#     get_code_dependency(code, names = 'b'),
#     code_b_expected
#   )
#
#   testthat::expect_identical(
#     get_code_dependency(code, names = 'c'),
#     code_c_expected
#   )
#
#   testthat::expect_identical(
#     get_code_dependency(code_2, names = 'b'),
#     code_b_expected_2
#   )
#
#   testthat::expect_identical(
#     get_code_dependency(code_3, names = 'b'),
#     code_b_expected_3
#   )
#
#   testthat::expect_identical(
#     get_code_dependency(code_4, names = 'classes'),
#     code_c_expected_4
#   )
#
# })
#
# testthat::test_that("remove_graph_duplicates cleans graph out ouf duplicates", {
#   graph <- list(
#     c("a", "a", ":", "b"),
#     c("a", ":", "b", "b"),
#     c("a", ":", "a")
#   )
#   testthat::expect_equal(
#     remove_graph_duplicates(graph),
#     list(
#       c("a", ":", "b"),
#       c("a", ":", "b"),
#       c("a", ":", "a")
#     )
#   )
#
# })
