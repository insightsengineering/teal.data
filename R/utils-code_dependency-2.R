

code_graph <- function(calls_pd) {

  # TODO
  # based on
  # R/utils-code_dependency.R detect_symbol
  # R/utils-code_dependency.R used_in_function
  # R/utils-code_dependency.R code_dependency()->cooccurrence

  # final_list is a list()
  # length = length(calls_pd)
  # structure - character(n)
  #           - (1) character(n) # occurence  OR side_effectS
  #           - (2) character(n) # occurence AND side_effectS
  #           - (3) character(n) # occurence  OR side_effectS  and influencers
  #           - (5) character(n) # occurence AND side_effectS  and influencers
  #           - (6) character(n) #                            only influencers
  #
  #           - occurence can be character(n), e.g. "a <- b <- 5", "a <- iris"                # a, b / a
  #           - side_effectS  is character(n), e.g. "@linktso ADSL", "@linktso ADSL ADLB"     # ADSL / ADSL ADLB
  #           - influencers   is character(n), e.g. "a <- iris", "a <- setdiff(letters, 'd')" # iris / letters, b
  #
  # we need a separator to differentiate between influencers
  # proposition:
  # character(n) containing ":" element that divides occurence and side_effects from influencers
  # for case with only influencers then it is c(":", "influencer_name")

}

get_code_dependency <- function(code, names) {
  assert_classes(code, names)

  if (is_empty(code)) return(code)

  code <- assert_code(code)
  pd <- create_pd(code)
  assert_names(names, pd)

  calls_pd <- extract_calls(pd)

  graph <- code_graph(calls_pd)
  indexes <- sapply(names, function(x) graph_parser(x, graph))
  calls_pd[indexes] # or parsed_code[indexes] which is created in create_calls_pd # NOT SURE YET
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
    lapply(graph[which(occurence)], function(call) {
      strsplit(call, split = ":", fixed = TRUE)[[1]][2]
    })
  influencers <- setdiff(influencers, skip)

  # returns: logical
  influencers_deps <-
    lapply(influencers, function(influencer){
      graph_parser(influencers, graph[1:max(which(occurence))], skip) ##### NOTE (2) BELOW
    })

  unique(which(occurence), which(influencers_deps))

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

# example code

code <- "

  a <- 5
  b <- a + 3
  a <- a + 6
  5 -> c @linksto a

"

graph <- list(
  c("a"),
  c("b", ":", "a"),
  c("a"),              # or c("a", ":", "a")
  c("c", "a", ":", "5")
)

