.onLoad <- function(libname, pkgname) { # nolint
  # expose default CDISC dataset names
  # copy from excel file
  default_cdisc_keys <- yaml::yaml.load_file(
    get_package_file("teal.data", "cdisc_datasets/cdisc_datasets.yaml")
  ) # nolint
  assign("default_cdisc_keys", default_cdisc_keys, envir = parent.env(environment()))

  # Set up the teal logger instance
  teal.logger::register_logger("teal.data")

  invisible()
}
