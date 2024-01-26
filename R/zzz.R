.onLoad <- function(libname, pkgname) {
  # expose default CDISC dataset names
  # copy from excel file
  default_cdisc_keys <- yaml::yaml.load_file(
    system.file("cdisc_datasets/cdisc_datasets.yaml", package = "teal.data")
  )
  assign("default_cdisc_keys", default_cdisc_keys, envir = parent.env(environment()))

  invisible()
}

# use non-exported function from teal.code
lang2calls <- getFromNamespace("lang2calls", "teal.code")
