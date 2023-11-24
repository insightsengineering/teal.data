

code_graph <- function(calls_pd) {

  # below are 3 lists, length = length(calls_pd)
  cooccurence <- extract_occurence(calls_pd)

  side_effects <- extract_side_effects(calls_pd)

  final_list <- append_side_effects(cooccurence, side_effects)

  # structure - character(n)
  #           - (1) character(n) # occurence  OR side_effectS
  #           - (2) character(n) # occurence AND side_effectS
  #           - (3) character(n) # occurence  OR side_effectS  and influencers
  #           - (5) character(n) # occurence AND side_effectS  and influencers
  #           - (6) character(n) #                            only influencers  - is this possible?
  #
  #           - occurence can be character(n), e.g. "a <- b <- 5", "a <- iris"                # a, b / a
  #           - side_effectS  is character(n), e.g. "@linktso ADSL", "@linktso ADSL ADLB"     # ADSL / ADSL ADLB
  #           - influencers   is character(n), e.g. "a <- iris", "a <- setdiff(letters, 'd')" # iris / letters, b
  #
  # we need a separator to differentiate between influencers
  # proposition:
  # character(n) containing ":" element that divides occurence and side_effects from influencers
  # for case with only influencers then it is c(":", "influencer_name") - is this possible?
  final_list
}

extract_occurence <- function(calls_pd) {

  # TO BE POLISHED:
  # CAN
  # R/utils-code_dependency.R used_in_function
  # R/utils-code_dependency.R code_dependency()
  # HELP?

  lapply(
    calls_pd,
    function(x) {
      # CAN THIS BE SIMPLIFIED?
      # x %in% "SYMBOL", "SYMBOL_FUNCTION_CALL"
      # and x not %in% "SYMBOL_FORMALS"
      # DO I STILL NEED & x$text %in% names
      sym_cond <- which(x$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL"))# & x$text %in% names)
      sym_form_cond <- which(x$token == "SYMBOL_FORMALS")# & x$text %in% names)
      sym_cond <- sym_cond[!x[sym_cond, "text"] %in% x[sym_form_cond, "text"]]

      # watch out for SYMBOLS after $ and @, e.g. x$a x@a // x is object, a is not
      # for x$a, a's ID is $'s ID-2
      # so we need to remove all IDs that have ID = $ID - 2
      object_ids <- x[sym_cond, "id"]
      dollar_ids <- x[x$token %in% c("'$'", "'@'"), "id"]
      after_dollar <- object_ids[(object_ids - 2) %in% dollar_ids]
      sym_cond <- setdiff(sym_cond, which(x$id %in% after_dollar))

      # if there was an assignment operation
      if (length(sym_cond) >= 2) {
        ass_cond <- grep("ASSIGN", x$token)
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
      ##### NOTE(3): WHAT IS THERE ARE 2+ ASSIGNEMNTS, e.g. a <- b <- c
      ##### NOTE(3): WHAT IS THERE ARE 2+ ASSIGNEMNTS, e.g. a <- b -> c
    }
  )
}

extract_side_effects <- function(calls_pd) {
  lapply(
    calls_pd,
    function(x) {
      linksto <- grep("@linksto", x[x$token == "COMMENT", "text"], value = TRUE)
      unlist(strsplit(sub("\\s*#\\s*@linksto\\s+", "", linksto), "\\s+"))
    }
  )
}

append_side_effects <- function(cooccurence, side_effects) {
  mapply(c, side_effects, cooccurence, SIMPLIFY = FALSE)
}


get_code_dependency <- function(code, names) {
  assert_classes(code, names)

  if (is_empty(code)) return(code)

  code <- assert_code(code)
  pd <- create_pd(code)
  assert_names(names, pd)

  calls_pd <- extract_calls(pd)

  graph <- code_graph(calls_pd)
  indexes <- unlist(lapply(names, function(x) graph_parser(x, graph)))
  as.character(code[indexes])
}

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
  lapply(pd[pd$parent == 0, "id"], get_children, pd = pd)
}

graph_parser <- function(x, graph, skip = NULL) {

  skip <- c(x, skip)

  # returns: logical
  occurence <- ##### NOTE (1) BELOW
    vapply(graph, function(call) {
      objects <- strsplit(call, split = ":", fixed = TRUE)[[1]][1]
      x %in% objects
    },
    logical(1)
  )

  # returns: character
  influencers <- #
    unlist(
      lapply(graph[occurence], function(call) {
        call[which(":" == call)+1]
        #strsplit(call, split = ":", fixed = TRUE)[[1]][2]
      })
    )
  influencers <- setdiff(influencers, skip)

  # STOPPED HERE: CONTINUE FROM HERE - almost there!
  if (length(influencers)) {
    # returns: logical
    influencers_deps <-
      lapply(influencers, function(influencer){
        graph_parser(influencers, graph[1:max(which(occurence))], skip) ##### NOTE (2) BELOW
      })

    unique(which(occurence), which(influencers_deps))
  } else {
    which(occurence)
  }

  ##### NOTE (1)
  # DOUBLE CHECK: Does it also return side effects created after the object was used?
  ##### NOTE (2)
  # What about side effects for influencers that are in higher lines than max(which(occurence))

}

assert_classes <- function(code, names) {
  checkmate::assert_multi_class(code, classes = c("character", "expression"))
  checkmate::assert_character(names)
}

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

assert_names <- function(names, pd) {
  symbols <- unique(pd[pd$token == "SYMBOL", "text"])
  if (!all(names %in% symbols)) {
    warning(
      "Object(s) not found in code: ",
      toString(setdiff(names, symbols))
    )
  }
}

create_pd <- function(code) {
  utils::getParseData(code)
}

is_empty <- function(code){
  identical(code, character(0)) || identical(trimws(code), "")
}


# example ---------------------------------------------------------------------------------------------------------

code <- "

  a <- 5
  b <- a + 3
  a <- a + 6
  5 -> c # @linksto a

"

graph_expected <- list(
  c("a"),
  c("b", ":", "a"),
  c("a", ":", "a"),
  c("a", "c")
)

code_a_expected <- c("a <- 5", "a <- a + 6", "c <- 5")
code_b_expected <- c("a <- 5", "b <- a + 3")
code_c_expected <- c("c <- 5")

testthat::test_that("code_graph returns proper structure of the dependency graph", {

  code <- assert_code(code)
  pd <- create_pd(code)
  calls_pd <- extract_calls(pd)
  graph <- code_graph(calls_pd)
  testthat::expect_identical(
    graph,
    graph_expected
  )

})

testthat::test_that("graph_parser returns proper code based on code_graph", {

  code <- assert_code(code)
  pd <- create_pd(code)
  calls_pd <- extract_calls(pd)
  graph <- code_graph(calls_pd)
  names <- 'a'
  indexes <- unlist(lapply(names, function(x) graph_parser(x, graph)))
  testthat::expect_identical(
    as.character(code[indexes]),
    code_a_expected
  )

})

testthat::test_that("get_code_dependency returns proper code", {

  testthat::expect_identical(
    get_code_dependency(code, names = 'a'),
    code_a_expected
  )

  # TO BE FIXED:
  testthat::skip({
    testthat::expect_identical(
      get_code_dependency(code, names = 'b'),
      code_b_expected
    )

    testthat::expect_identical(
      get_code_dependency(code, names = 'C'),
      code_c_expected
    )
  })

})




