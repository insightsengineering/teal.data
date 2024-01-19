# Register method for testthat::expect_identical to compare join_keys
if (nzchar(Sys.getenv("TESTTHAT"))) {
  registerS3method("all.equal", "join_keys", all.equal.join_keys)
}
