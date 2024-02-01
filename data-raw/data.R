## code to prepare `data` for testing examples
library(scda)
rADAE <- synthetic_cdisc_data("latest")$adae # nolint: object_name_linter.
usethis::use_data(rADAE)

rADCM <- synthetic_cdisc_data("latest")$adcm # nolint: object_name_linter.
usethis::use_data(rADCM)

rADEX <- synthetic_cdisc_data("latest")$adex # nolint: object_name_linter.
usethis::use_data(rADEX)

rADLB <- synthetic_cdisc_data("latest")$adlb # nolint: object_name_linter.
usethis::use_data(rADLB)

rADRS <- synthetic_cdisc_data("latest")$adrs # nolint: object_name_linter.
usethis::use_data(rADRS)

rADSL <- synthetic_cdisc_data("latest")$adsl # nolint: object_name_linter.
usethis::use_data(rADSL)

rADTR <- synthetic_cdisc_data("latest")$adtr # nolint: object_name_linter.
usethis::use_data(rADTR)

rADTTE <- synthetic_cdisc_data("latest")$adtte # nolint: object_name_linter.
usethis::use_data(rADTTE)

rADVS <- synthetic_cdisc_data("latest")$advs # nolint: object_name_linter.
usethis::use_data(rADVS)

## Code to prepare default CDISC datasets for `teal.data`
default_cdisc_keys <- yaml::yaml.load_file(file.path("data-raw", "cdisc_datasets.yaml"))

default_cdisc_join_keys <- teal.data::join_keys()
for (name in names(default_cdisc_keys)) {
  # Set default primary keys
  keys_list <- default_cdisc_keys[[name]]

  if (!is.null(keys_list[["primary"]])) {
    default_cdisc_join_keys[[name]][[name]] <- keys_list[["primary"]]
  }

  if (!is.null(keys_list[["parent"]])) {
    if (!is.null(keys_list[["foreign"]])) {
      default_cdisc_join_keys[[name]][[keys_list[["parent"]]]] <- keys_list[["foreign"]]
    }
    teal.data::parents(default_cdisc_join_keys)[[name]] <- keys_list[["parent"]]
  }
}

usethis::use_data(default_cdisc_join_keys)
