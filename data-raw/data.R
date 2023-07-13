## code to prepare `data` for testing examples
library(scda)
rADAE <- synthetic_cdisc_data("latest")$adae # nolint
usethis::use_data(rADAE)

rADCM <- synthetic_cdisc_data("latest")$adcm # nolint
usethis::use_data(rADCM)

rADEX <- synthetic_cdisc_data("latest")$adex # nolint
usethis::use_data(rADEX)

rADLB <- synthetic_cdisc_data("latest")$adlb # nolint
usethis::use_data(rADLB)

rADRS <- synthetic_cdisc_data("latest")$adrs # nolint
usethis::use_data(rADRS)

rADSL <- synthetic_cdisc_data("latest")$adsl # nolint
usethis::use_data(rADSL)

rADTR <- synthetic_cdisc_data("latest")$adtr # nolint
usethis::use_data(rADTR)

rADTTE <- synthetic_cdisc_data("latest")$adtte # nolint
usethis::use_data(rADTTE)

rADVS <- synthetic_cdisc_data("latest")$advs # nolint
usethis::use_data(rADVS)
