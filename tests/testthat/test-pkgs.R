test_that("pkgs can be constructed with no input", {
  ps <- pkgs()
  expect_equal(length(ps), 0)
  expect_s3_class(ps, "pkgs")
})

test_that("pkgs can be constructed with a single package", {
  ps <- pkgs("dplyr")
  expect_equal(length(ps), 1)
  expect_s3_class(ps, "pkgs")
  expect_s3_class(ps[[1]], "pkg")
})

test_that("pkgs can be constructed with two arguments", {
  ps <- pkgs("dplyr", "dose/nmrunner")
  expect_equal(length(ps), 2)
  expect_s3_class(ps, "pkgs")
  expect_s3_class(ps[[1]], "pkg")
  expect_s3_class(ps[[2]], "pkg")
})

test_that("pkgs are be constructed with arguments and vectors", {
  ps <- pkgs("dplyr", c("dose/nmrunner", "tidyr"))
  expect_equal(length(ps), 3)
  expect_s3_class(ps, "pkgs")
  expect_s3_class(ps[[1]], "pkg")
  expect_s3_class(ps[[2]], "pkg")
  expect_s3_class(ps[[3]], "pkg")
})

test_that("pkgs fails if an argument is not a character", {
  expect_error(pkgs(1))
})

test_that("pkgs shortcut functions work", {
  ps_local <- pkgs_local("dplyr")
  expect_equal(ps_local[[1]]$install, "local")
  ps_container <- pkgs_container("dplyr")
  expect_equal(ps_container[[1]]$install, "container")
})

test_that("pkgs can be converted to a data frame", {
  ps <- pkgs("dplyr", "dose/nmrunner")
  df <- as.data.frame(ps)
  expect_equal(nrow(df), 2)
  expect_equal(ncol(df), 6)

  empty_ps <- pkgs()
  empty_df <- as.data.frame(empty_ps)
  expect_equal(nrow(empty_df), 0)
})

test_that("pkgs can be printed", {
  ps <- pkgs("dplyr", "dose/nmrunner")
  expect_output(print(ps), "dplyr")
  expect_output(print(ps), "nmrunner")

  empty_ps <- pkgs()
  expect_output(print(empty_ps), "No packages")

  expect_output(print(ps, table = TRUE), "dplyr")
})
