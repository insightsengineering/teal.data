# Register method for testthat::expect_identical to compare join_keys
if (testthat::is_testing()) {
  registerS3method("all.equal", "join_keys", all.equal.join_keys)
}
