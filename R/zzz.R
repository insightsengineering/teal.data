.onLoad <- function(libname, pkgname) { # nolint
  # expose default CDISC dataset names
  # copy from excel file
  default_cdisc_keys <- yaml::yaml.load_file(
    get_package_file("teal.data", "cdisc_datasets/cdisc_datasets.yaml")
  )
  assign("default_cdisc_keys", default_cdisc_keys, envir = parent.env(environment()))

  # update default_cdisc_join_keys
  assign(
    "default_cdisc_join_keys",
    build_cdisc_join_keys(default_cdisc_keys),
    envir = parent.env(environment())
  )

  # Set up the teal logger instance
  teal.logger::register_logger("teal.data")

  invisible()
}

# use non-exported function from teal.code
format_expression <- getFromNamespace("format_expression", "teal.code")
get_code_dependency <- getFromNamespace("get_code_dependency", "teal.code")
