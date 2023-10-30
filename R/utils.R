#' Get code from script
#'
#' Get code from script. Switches between `code` and `script` arguments
#' to return non-empty one to pass it further to constructors.
#'
#' @param code (`character`)\cr
#'   an R code to be evaluated or a `PythonCodeClass` created using [python_code].
#' @inheritParams dataset_connector
#' @return code (`character`)
#' @keywords internal
code_from_script <- function(code, script, dataname = NULL) {
  checkmate::assert(
    checkmate::check_character(code, max.len = 1, any.missing = FALSE),
    checkmate::check_class(code, "PythonCodeClass")
  )
  checkmate::assert_character(script, max.len = 1, any.missing = FALSE)
  if (length(code) == 0 && length(script) == 0) {
    return(character(0))
  }

  if (checkmate::test_string(code) && checkmate::test_string(script)) {
    stop("Function doesn't accept 'code' and 'script' at the same time.
         Please specify either 'code' or 'script'", call. = FALSE)
  }

  if (checkmate::test_string(script)) {
    code <- read_script(file = script, dataname = dataname)
  }

  code
}

#' Read .R file into character
#'
#' @description `r lifecycle::badge("stable")`
#' Comments will be excluded
#'
#' @param file (`character`) File to be parsed into code
#' @param dataname (`character`) dataset name to subset code from chunks
#' @return (`character`) vector with the code
#'
#' @export
#' @examples
#' file_example <- tempfile()
#' writeLines(c("x <- 2", "#second line comment", "x <- x + 2"), file_example)
#'
#' read_script(file_example)
read_script <- function(file, dataname = NULL) {
  checkmate::assert_string(file)
  checkmate::assert_file_exists(file)
  paste(
    code_exclude(
      enclosed_with_dataname(
        get_code_single(file, read_sources = TRUE),
        dataname = dataname
      ),
      exclude_comments = TRUE
    ),
    collapse = "\n"
  )
}

#' Function to get a file out of a package
#'
#' @param pkg (`character`)\cr
#'  The name of the package the file should be received from.
#' @param file_name (`character`)\cr
#'  The name of the file to be received or path to it starting from
#'  the base package path.
#' @return The path to the file
#' @keywords internal
#' @examples
#' teal.data:::get_package_file("teal.data", "WORDLIST")
#' teal.data:::get_package_file("teal.data", "cdisc_datasets/cdisc_datasets.yaml")
get_package_file <- function(pkg = NULL, file_name = NULL) {
  checkmate::assert_string(pkg)
  checkmate::assert_string(file_name)
  base_file <- system.file(file_name, package = pkg)

  if (file.exists(base_file)) {
    return(base_file)
  } else {
    stop(paste("There is no such file:", file_name, "or package:", pkg))
  }
}

# Function to be used while trying to load the object of specific class from the script.
object_file <- function(path, class) {
  checkmate::assert_string(path)
  checkmate::assert_file_exists(path)
  checkmate::assert_string(class)

  lines <- paste0(readLines(path), collapse = "\n")
  object <- eval(parse(text = lines, keep.source = FALSE))

  if (!inherits(object, class)) {
    stop("The object returned from the file is not of ", class, " class.")
  }
  return(object)
}

#' Check if package can be loaded
#'
#' @param pckg `character` package name.
#' @param msg `character` error message to display if package is not available.
#'
#' @return Error or invisible NULL.
#' @keywords internal
check_pkg_quietly <- function(pckg, msg) {
  checkmate::assert_string(pckg)
  checkmate::assert_string(msg)
  if (!pckg %in% rownames(utils::installed.packages())) {
    stop(msg)
  }

  invisible(NULL)
}


#' validate metadata as a list of length one atomic entries (or NULL)
#' @param metadata `object` to be checked
#' @return `NULL` or throw error
#' @examples
#'
#' validate_metadata(NULL)
#' validate_metadata(list(A = TRUE, B = 10, C = "test"))
#' \dontrun{
#' validate_metadata(list(a = 1:10))
#' }
#'
#' @export
validate_metadata <- function(metadata) {
  checkmate::assert_list(metadata, any.missing = FALSE, names = "named", null.ok = TRUE)
  lapply(names(metadata), function(name) {
    checkmate::assert_atomic(metadata[[name]], len = 1, .var.name = name)
  })
  return(NULL)
}

#' Resolve the expected bootstrap theme
#' @keywords internal
get_teal_bs_theme <- function() {
  bs_theme <- getOption("teal.bs_theme")
  if (is.null(bs_theme)) {
    NULL
  } else if (!inherits(bs_theme, "bs_theme")) {
    warning("teal.bs_theme has to be of a bslib::bs_theme class, the default shiny bootstrap is used.")
    NULL
  } else {
    bs_theme
  }
}
