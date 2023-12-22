# get_code_dependency ----

#' Get Code Dependency of an Object
#'
#' Extract subset of code required to reproduce specific object(s), including code producing side-effects.
#'
#' Given a character vector with code, this function will extract the part of the code responsible for creating
#' the variables specified by `names`.
#' This includes the final call that creates the variable(s) in question as well as all _parent calls_,
#' _i.e._ calls that create variables used in the final call and their parents, etc.
#' Also included are calls that create side-effects like establishing connections.
#'
#' It is assumed that object dependency is established by using three assignment operators: `<-`, `=`, and `->` .
#' Other assignment methods (`assign`, `<<-`) or non-standard-evaluation methods are not supported.
#'
#' Side-effects are not detected automatically and must be marked in the code.
#' Add `# @linksto object` at the end of a line where a side-effect occurs to specify that this line is required
#' to reproduce a variable called `object`.
#'
#' @param code `character` with the code.
#' @param names `character` vector of object names.
#' @param check_names `logical(1)` flag specifying if a warning for non-existing names should be displayed.
#'
#' @return Character vector, a subset of `code`.
#' Note that subsetting is actually done on the calls `code`, not necessarily on the elements of the vector.
#'
#' @keywords internal
get_code_dependency <- function(code, names, check_names = TRUE) {
  checkmate::assert_character(code)
  checkmate::assert_character(names, any.missing = FALSE)

  if (identical(code, character(0)) || identical(trimws(code), "")) {
    return(code)
  }

  code <- parse(text = code, keep.source = TRUE)
  pd <- utils::getParseData(code)

  if (check_names) {
    # Detect if names are actually in code.
    symbols <- unique(pd[pd$token == "SYMBOL", "text"])
    if (!all(names %in% symbols)) {
      warning("Object(s) not found in code: ", toString(setdiff(names, symbols)))
    }
  }

  calls_pd <- extract_calls(pd)

  graph <- code_graph(calls_pd)
  ind <- unlist(lapply(names, function(x) graph_parser(x, graph)))

  lib_ind <- detect_libraries(calls_pd)

  as.character(code[unique(c(lib_ind, ind))])
}

#' Split the result of `utils::getParseData()` into separate calls
#'
#' @param pd (`data.frame`) A result of `utils::getParseData()`.
#'
#' @return
#' A `list` of `data.frame`s.
#' Each element is a subset of `pd` corresponding to one call in the original code from which `pd` was obtained.
#' Only four columns (`"token"`, `"text"`, `"id"`, `"parent"`) are kept, the rest is discarded.
#'
#' @keywords internal
#' @noRd
extract_calls <- function(pd) {
  calls <- lapply(pd[pd$parent == 0, "id"], get_children, pd = pd)
  fix_comments(calls)
}

get_children <- function(pd, parent) {
  idx_children <- abs(pd$parent) == parent
  children <- pd[idx_children, c("token", "text", "id", "parent")]
  if (nrow(children) == 0) {
    return(NULL)
  }

  if (parent > 0) {
    do.call(rbind, c(list(children), lapply(children$id, get_children, pd = pd)))
  }
}

fix_comments <- function(calls) {
  # If the first token is a COMMENT, then it belongs to the previous call.
  if (length(calls) >= 2) {
    for (i in 2:length(calls)) {
      if (grepl("@linksto", calls[[i]][1, "text"])) {
        calls[[i - 1]] <- rbind(calls[[i - 1]], calls[[i]][1, ])
        calls[[i]] <- calls[[i]][-1, ]
      }
    }
  }
  calls
}

# code_graph ----

#' Create Object Dependencies Graph Within Parsed Code
#'
#' @description
#' Builds dependency graph that identifies dependencies between objects in parsed code.
#' Helps understand which objects depend on which.
#'
#' @param calls_pd `list` of `data.frame`s;
#'  result of `utils::getParseData()` split into subsets representing individual calls;
#'  created by `extract_calls()` function
#'
#' @return
#' A list (of length of input `calls_pd`) where each element represents one call.
#' Each element is a character vector listing names of objects that depend on this call
#' and names of objects that this call depends on.
#' Dependencies are listed after the `"<-"` string, e.g. `c("a", "<-", "b", "c")` means that in this call object `a`
#' depends on objects `b` and `c`.
#' If a call is tagged with `@linksto a`, then object `a` is understood to depend on that call.
#'
#' @keywords internal
#' @noRd
code_graph <- function(calls_pd) {
  cooccurrence <- extract_occurrence(calls_pd)

  side_effects <- extract_side_effects(calls_pd)

  mapply(c, side_effects, cooccurrence, SIMPLIFY = FALSE)
}

#' Extract Object Occurrence
#'
#' @description Extracts objects occurrence within calls passed by `calls_pd`.
#' Also detects which objects depend on which within a call.
#'
#' @param calls_pd `list` of `data.frame`s;
#'  result of `utils::getParseData()` split into subsets representing individual calls;
#'  created by `extract_calls()` function
#'
#' @return
#' A list (of length of input `calls_pd`) where each element represents one call.
#' Each element is a character vector listing names of objects that depend on this call
#' and names of objects that this call depends on.
#' Dependencies are listed after the `"<-"` string, e.g. `c("a", "<-", "b", "c")` means that in this call object `a`
#' depends on objects `b` and `c`.
#' If a call is tagged with `@linksto a`, then object `a` is understood to depend on that call.
#'
#' @keywords internal
#' @noRd
extract_occurrence <- function(calls_pd) {
  is_in_function <- function(x) {
    # If an object is a function parameter,
    # then in calls_pd there is a `SYMBOL_FORMALS` entry for that object.
    function_id <- x[x$token == "FUNCTION", "parent"]
    if (length(function_id)) {
      x$id %in% get_children(x, function_id)$id
    } else {
      rep(FALSE, nrow(x))
    }
  }
  lapply(
    calls_pd,
    function(call_pd) {
      # Handle data(object)/data("object")/data(object, envir = ) independently.
      data_call <- call_pd$token == "SYMBOL_FUNCTION_CALL" & call_pd$text == "data"
      if (any(data_call)) {
        sym <- call_pd[which(data_call) + 1, "text"]
        return(c(gsub("'", "", sym), "<-", "data"))
      }

      # What occurs in a function body is not tracked.
      x <- call_pd[!is_in_function(call_pd), ]
      sym_cond <- which(x$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL"))

      if (length(sym_cond) == 0) {
        return(character(0))
      }
      # Watch out for SYMBOLS after $ and @. For x$a x@a: x is object, a is not.
      # For x$a, a's ID is $'s ID-2 so we need to remove all IDs that have ID = $ID - 2.
      dollar_ids <- x[x$token %in% c("'$'", "'@'"), "id"]
      if (length(dollar_ids)) {
        object_ids <- x[sym_cond, "id"]
        after_dollar <- object_ids[(object_ids - 2) %in% dollar_ids]
        sym_cond <- setdiff(sym_cond, which(x$id %in% after_dollar))
      }

      # If there was an assignment operation detect direction of it.
      ass_cond <- grep("ASSIGN", x$token)
      if (length(ass_cond)) { # NOTE 1
        sym_cond <- sym_cond[sym_cond > ass_cond] # NOTE 2
      }
      if ((length(ass_cond) && x$text[ass_cond] == "->") || !length(ass_cond)) { # NOTE 3
        sym_cond <- rev(sym_cond)
      }
      append(unique(x[sym_cond, "text"]), "<-", after = 1)

      ### NOTE 3: What if there are 2+ assignments, e.g. a <- b -> c or e.g. a <- b <- c.
      ### NOTE 2: For cases like 'eval(expression(b <- b + 2))' removes 'eval(expression('.
      ### NOTE 1: Cases like 'data(iris)' that do not have an assignment operator.
      ### NOTE 1: Then they are parsed as c("iris", "<-", "data")
    }
  )
}

#' Extract Side Effects
#'
#' @description Extracts all object names from the code that are marked with `@linksto` tag.
#'
#' @details
#' The code may contain functions calls that create side effects, e.g. modify the environment.
#' Static code analysis may be insufficient to determine which objects are created or modified by such a function call.
#' The `@linksto` comment tag is introduced to mark a call as having a (side) effect on one or more objects.
#' With this tag a complete object dependency structure can be established.
#' Read more about side effects and the usage of `@linksto` tag in [`get_code_dependencies()`] function.
#'
#' @param calls_pd `list` of `data.frame`s;
#'  result of `utils::getParseData()` split into subsets representing individual calls;
#'  created by `extract_calls()` function
#'
#' @return
#' A list of length equal to that of `calls_pd`, where each element is a character vector of names of objects
#' depending a call tagged with `@linksto` in a corresponding element of `calls_pd`.
#'
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

# graph_parser ----

#' Return the indices of calls needed to reproduce an object
#'
#' @param x The name of the object to return code for.
#' @param graph A result of `code_graph()`.
#'
#' @return
#' Integer vector of indices that can be applied to `graph` to obtain all calls required to reproduce object `x`.
#'
#' @keywords internal
#' @noRd
graph_parser <- function(x, graph) {
  occurrence <- vapply(
    graph, function(call) {
      ind <- match("<-", call, nomatch = length(call) + 1L)
      x %in% call[seq_len(ind - 1L)]
    },
    logical(1)
  )

  dependencies <- lapply(graph[occurrence], function(call) {
    ind <- match("<-", call, nomatch = 0L)
    call[(ind + 1L):length(call)]
  })
  dependencies <- setdiff(unlist(dependencies), x)

  if (length(dependencies) && any(occurrence)) {
    dependency_ids <- lapply(dependencies, function(dependency) {
      graph_parser(dependency, graph[1:max(which(occurrence))])
    })
    sort(unique(c(which(occurrence), unlist(dependency_ids))))
  } else {
    which(occurrence)
  }
}


# default_side_effects --------------------------------------------------------------------------------------------

#' Detect Library Calls
#'
#' Detects `library()` and `require()` function calls.
#'
#' @param calls_pd `list` of `data.frame`s;
#'  result of `utils::getParseData()` split into subsets representing individual calls;
#'  created by `extract_calls()` function
#'
#' @return
#' Integer vector of indices that can be applied to `graph` (result of `code_graph()`) to obtain all calls containing
#' `library()` or `require()` calls that are always returned for reproducibility.
#'
#' @keywords internal
#' @noRd
detect_libraries <- function(calls_pd) {
  defaults <- c("library", "require")

  which(
    vapply(
      calls_pd,
      function(call) {
        any(call$token == "SYMBOL_FUNCTION_CALL" & call$text %in% defaults)
      },
      logical(1)
    )
  )
}
