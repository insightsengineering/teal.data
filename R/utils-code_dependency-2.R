

code_graph <- function(calls_pd) {

  final_list <- symbols_by_calls(calls_pd)

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

symbols_by_calls <- function(calls_pd) {
  # TODO
  # based on
  # R/utils-code_dependency.R detect_symbol
  # R/utils-code_dependency.R used_in_function
  # R/utils-code_dependency.R code_dependency()->cooccurrence
}

get_code_dependency <- function(code, names) {
  assert_classes()
  assert_names()
  code_assertions() # parse character / verify expression (if has 'srcref' attribute)

  calls_pd <- create_calls_pd(code)

  graph <- code_graph(calls_pd)
  indexes <- sapply(names, function(x) graph_parser(x, graph))
  calls_pd[indexes] # or parsed_code[indexes] which is created in create_calls_pd # NOT SURE YET
}

create_calls_pd <- function(code) {
  parsed_code <- parse(text = code, keep.source = TRUE)
  pd <- utils::getParseData(parsed_code)
  extract_calls(pd) # R/utils-code_dependency.R get_children
}

graph_parser <- function(x, graph) {

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

  # returns: logical
  influencers_deps <-
    lapply(influencers, function(influencer){
      graph_parser(influencer, graph[1:max(which(occurence))]) ##### NOTE (2) BELOW
    })

  unique(which(occurence), which(influencers_deps))

  ##### NOTE (1)
  # DOUBLE CHECK: Does it also return side effects created after the object was used?
  ##### NOTE (2)
  # What about side effects for influencers that are in higher lines than max(which(occurence))

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

