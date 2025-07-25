## code to prepare `data` for testing examples
rADAE <- random.cdisc.data::cadae # nolint: object_name.
usethis::use_data(rADAE)

rADCM <- random.cdisc.data::cadcm # nolint: object_name.
usethis::use_data(rADCM)

rADEX <- random.cdisc.data::cadex # nolint: object_name.
usethis::use_data(rADEX)

rADLB <- random.cdisc.data::cadlb # nolint: object_name.
usethis::use_data(rADLB)

rADRS <- random.cdisc.data::cadrs # nolint: object_name.
usethis::use_data(rADRS)

rADSL <- random.cdisc.data::cadsl # nolint: object_name.
usethis::use_data(rADSL)

rADTR <- random.cdisc.data::cadtr # nolint: object_name.
usethis::use_data(rADTR)

rADTTE <- random.cdisc.data::cadtte # nolint: object_name.
usethis::use_data(rADTTE)

rADVS <- random.cdisc.data::cadvs # nolint: object_name.
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
