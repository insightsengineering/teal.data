# get_code_dependency ----

#' Return the lines of code (with side-effects) needed to reproduce the object
#'
#' @details This function assumes that object relationships are established using the `<-`, `=`, or `->` assignment
#' operators. It does not support other object creation methods like `assign` or `<<-`, nor non-standard-evaluation
#' methods. To specify relationships between side-effects and objects, you can use the comment tag
#' `# @linksto object_name` at the end of a line where the side-effect occurs.
#'
#' @param code `character` with the code.
#' @param names `character` vector of object names.
#'
#' @return `character` vector of elements of `code` calls that were required to build the side-effects and
#' influencing objects having and impact on the `object`
#'
#' @keywords internal
get_code_dependency <- function(code, names) {
  checkmate::assert_character(code)
  checkmate::assert_character(names, any.missing = FALSE)

  if (identical(code, character(0)) || identical(trimws(code), "")) {
    return(code)
  }

  code <- parse(text = code, keep.source = TRUE)
  pd <- utils::getParseData(code)

  # Detect if names are actually in code.
  symbols <- unique(pd[pd$token == "SYMBOL", "text"])
  if (!all(names %in% symbols)) {
    warning("Object(s) not found in code: ", toString(setdiff(names, symbols)))
  }

  calls_pd <- extract_calls(pd)

  graph <- code_graph(calls_pd)
  ind <- unlist(lapply(names, function(x) graph_parser(x, graph)))
  as.character(code[unique(ind)])
}

#' Split the result of `utils::getParseData()` into separate calls
#' @param pd (`data.frame`) A result of `utils::getParseData()`.
#'
#' @return
#' A `list` of `data.frame`s. Each element is a subset of `pd` corresponding to one call in the original code
#' from which `pd` was obtained. Only four columns (`"token"`, `"text"`, `"id"`, `"parent"`) are kept, the rest is discarded.
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
#' @description This function constructs a dependency graph that identifies the relationships between objects in
#' parsed code. It helps you understand which objects are needed to recreate a specific object.
#'
#' @param calls_pd A `list` of `data.frame`s, which is a result of `utils::getParseData()` grouped into separate calls.
#' A result of `extract_calls()` function.
#'
#' @return A `list` (of length of input `calls_pd`) where each element represents one call. Each element consists of a
#' character vector containing names of objects that were affected by this call, and names of objects influencing this
#' call. Influencers appear after `"<-"` string, e.g. `c("a", "<-", "b", "c")` means a call affected object named `a`,
#' and the object was affected by objects named `b` and `c`. If an object was marked with `@linksto` side effects tag
#' then it appears as an affected object at the beginning of a vector.
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
#' @description Extract objects occurrence within calls passed by `calls_pd`. It also detects which objects are
#' affected by others within a call, and which are influencers.
#'
#' @param calls_pd A `list` of `data.frame`s, which is a result of `utils::getParseData()` grouped into separate calls.
#' A result of `extract_calls()` function.
#' @return A `list` (of length of input `calls_pd`) where each element represents one call. Each element consists of a
#' character vector containing names of objects that were affected by this call, and names of objects influencing this
#' call. Influencers appear after `"<-"` string, e.g. `c("a", "<-", "b", "c")` means a call affected object named `a`,
#' and the object was affected by objects named `b` and `c`.
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

      # What occurs in a function body is not tracked.
      x <- call_pd[!is_in_function(call_pd), ]
      sym_cond <- which(x$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL"))

      if (length(sym_cond) == 0) {return(character(0))}
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
      if (length(ass_cond)) { # NOTE(1)
        sym_cond <- sym_cond[sym_cond > ass_cond] # NOTE(2)
      }
      if ((length(ass_cond) && x$text[ass_cond] == "->") || !length(ass_cond)) { # NOTE(3)
          sym_cond <- rev(sym_cond)
      }
      append(unique(x[sym_cond, "text"]), "<-", after = 1)

      ### NOTE(3): What if there are 2+ assignments, e.g. a <- b -> c or e.g. a <- b <- c.
      ### NOTE(2): For cases like 'eval(expression(b <- b + 2))' removes 'eval(expression('.
      ### NOTE(1): Cases like 'data(iris)' that do not have an assignment operator.
      ### NOTE(1): Then they are parsed as c("iris", "<-", "data")
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
#' @return A `list` of length equal to the length of `calls_pd` of character vectors of names of objects that are
#' affected by `@linksto` tag in a corresponding `call` element of `calls_pd`.
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

#' Return the indexes of calls of code needed to reproduce the object
#'
#' @param x The name of the object to return code for.
#' @param graph A result of `code_graph()`.
#'
#' @return `numeric` vector indicating which calls of `graph` are required to build the object passed by name in `x`.
#'
#' @keywords internal
#' @noRd
graph_parser <- function(x, graph) {

  occurrence <-
    vapply(
      graph, function(call) {
        ind <- match("<-", call, nomatch = length(call) +1L)
        x %in% call[seq_len(ind - 1L)]
      },
      logical(1)
    )

  influencers <-
    unlist(
      lapply(graph[occurrence], function(call) {
        if ("<-" %in% call) {
          ind <- match("<-", call)
          call[(ind + 1L):length(call)]
        }
      })
    )
  influencers <- setdiff(influencers, x)

  if (length(influencers) && any(occurrence)) {
    influencers_ids <-
      lapply(influencers, function(influencer) {
        graph_parser(influencer, graph[1:max(which(occurrence))])
      })

    sort(unique(c(which(occurrence), unlist(influencers_ids))))
  } else {
    which(occurrence)
  }
}
