# get_code_dependency ----

#' Get code dependency of an object
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
  calls_pd <- extract_calls(pd)

  if (check_names) {
    # Detect if names are actually in code.
    symbols <- unlist(lapply(calls_pd, function(call) call[call$token == "SYMBOL", "text"]))
    if (any(pd$text == "assign")) {
      assign_calls <- Filter(function(call) find_call(call, "assign"), calls_pd)
      ass_str <- unlist(lapply(assign_calls, function(call) call[call$token == "STR_CONST", "text"]))
      ass_str <- gsub("^['\"]|['\"]$", "", ass_str)
      symbols <- c(ass_str, symbols)
    }
    if (!all(names %in% unique(symbols))) {
      warning("Object(s) not found in code: ", toString(setdiff(names, symbols)))
    }
  }

  graph <- code_graph(calls_pd, code)
  ind <- unlist(lapply(names, function(x) graph_parser(x, graph)))

  lib_ind <- detect_libraries(calls_pd)

  as.character(code[unique(c(lib_ind, ind))])
}

#' Locate function call token
#'
#' Determine which row of parsed data is specific `SYMBOL_FUNCTION_CALL` token.
#'
#' Useful for determining occurrence of `assign` or `data` functions in an input call.
#'
#' @param call_pd `data.frame` as returned by `extract_calls()`
#' @param text `character(1)` to look for in `text` column of `call_pd`
#'
#' @return
#' Single integer specifying row in `call_pd` where `token` is `SYMBOL_FUNCTION_CALL` and `text` is `text`.
#' 0 if not found.
#'
#' @keywords internal
#' @noRd
find_call <- function(call_pd, text) {
  checkmate::check_data_frame(call_pd)
  checkmate::check_names(call_pd, must.include = c("token", "text"))
  checkmate::check_string(text)

  ans <- which(call_pd$token == "SYMBOL_FUNCTION_CALL" & call_pd$text == text)
  if (length(ans)) {
    ans
  } else {
    0L
  }
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
  calls <- Filter(Negate(is.null), calls)
  fix_comments(calls)
}

#' @keywords internal
#' @noRd
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

#' @keywords internal
#' @noRd
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

#' Create object dependencies graph within parsed code
#'
#' Builds dependency graph that identifies dependencies between objects in parsed code.
#' Helps understand which objects depend on which.
#'
#' @param calls_pd `list` of `data.frame`s;
#'  result of `utils::getParseData()` split into subsets representing individual calls;
#'  created by `extract_calls()` function
#' @param code `expression(n)` containing the parsed code
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
code_graph <- function(calls_pd, code) {
  cooccurrence <- extract_occurrence(calls_pd, code)

  side_effects <- extract_side_effects(calls_pd)

  mapply(function(x, y) unique(c(x, y)), side_effects, cooccurrence, SIMPLIFY = FALSE)
}

#' Extract object occurrence
#'
#' Extracts objects occurrence within calls passed by `calls_pd`.
#' Also detects which objects depend on which within a call.
#'
#' @param calls_pd `list` of `data.frame`s;
#'  result of `utils::getParseData()` split into subsets representing individual calls;
#'  created by `extract_calls()` function
#' @param code `expression(n)` containing the parsed code
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
extract_occurrence <- function(calls_pd, code) {
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
  occurrence <- lapply(
    calls_pd,
    function(call_pd) {
      # Handle data(object)/data("object")/data(object, envir = ) independently.
      data_call <- find_call(call_pd, "data")
      if (data_call) {
        sym <- call_pd[data_call + 1, "text"]
        return(c(gsub("^['\"]|['\"]$", "", sym), "<-"))
      }
      # Handle assign(x = ).
      # 0. x is the name of the first argument responsible for object name.
      # 1. Check if there was $token="SYMBOL_FUNCTION_CALL" & $text = "assign".
      # 2. Check if parameters were named $token="SYMBOL_SUB", "text"].
      # 3. Check first thing after 'assign' unless.
      # 3.a) x was in named parameters - then check it's position.
      # 3.b) x was not in named parameters - then check length(SYMBOL_SUB) + 1.
      # 4. Check if x was a string or a variable.
      # 4a). If x was a string return c("name", "<-").
      # 4b). If x was a variable return c("<-", "name").
      assign_call <- find_call(call_pd, "assign")
      if (assign_call) {
        # Check if parameters were named.
        if (any(call_pd$token == "SYMBOL_SUB")) {
          params <- call_pd[call_pd$token == "SYMBOL_SUB", "text"]
          pos <- match("x", params, nomatch = length(params) + 1L)
        } else {
          # Object is the first entry after 'assign'.
          pos <- 1
        }
        sym <- call_pd[assign_call + pos, "text"]
        is_variable <- call_pd[assign_call + pos, "token"] == "SYMBOL"
        ans <- c(gsub("^['\"]|['\"]$", "", sym), "<-")
        if (is_variable) ans <- rev(ans)
        return(ans)
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
      ### NOTE 1: Then they are parsed as c("iris", "<-") in final code graph.
    }
  )

  # If there is any entry that starts with "<-" in final code graph,
  # this means x parameter in assign function was a variable.
  # Then include x's value.
  if (length(occurrence) >= 2) {
    for (i in 2:length(occurrence)) {
      if (length(occurrence[[i]]) && occurrence[[i]][1] == "<-") {
        name <- occurrence[[i]][2]
        env <- new.env()
        eval(code[1:(i - 1)], envir = env)
        value <- get(name, envir = env)
        occurrence[[i]] <- c(value, occurrence[[i]])
      }
    }
  }
  occurrence
}

#' Extract side effects
#'
#' Extracts all object names from the code that are marked with `@linksto` tag.
#'
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

#' Detect library calls
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
