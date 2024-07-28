test_that("concat one package", {
  ps <- pkg("dplyr")
  c_ps <- c(ps)
  expect_s3_class(c_ps, "pkg")
})

test_that("concat two packages", {
  ps1 <- pkg("dplyr")
  ps2 <- pkg("dose/nmrunner")
  c_ps <- c(ps1, ps2)
  expect_s3_class(c_ps, "pkgs")
  expect_length(c_ps, 2)
  expect_identical(c_ps[[1]], ps1)
  expect_identical(c_ps[[2]], ps2)
})

test_that("concat one package and one pkgs", {
  ps1 <- pkg("dplyr")
  ps2 <- pkgs("dose/nmrunner", "tidyr")
  c_ps <- c(ps1, ps2)
  expect_s3_class(c_ps, "pkgs")
  expect_length(c_ps, 3)
  expect_identical(c_ps[2:3], ps2)
})

test_that("concat one package and one empty pkgs", {
  ps1 <- pkg("dplyr")
  ps2 <- pkgs()
  c_ps <- c(ps1, ps2)
  expect_s3_class(c_ps, "pkgs")
  expect_length(c_ps, 1)
  expect_identical(c_ps[[1]], ps1)
})

test_that("concat fails if wrong type is provided", {
  expect_error(c(pkg("dplyr"), 1))
  expect_error(c(pkg("dplyr"), list()))
})

test_that("pkgs: concat one pkg", {
  ps <- pkgs("dplyr")
  c_ps <- c(ps)
  expect_s3_class(c_ps, "pkgs")
  expect_length(c_ps, 1)
})

test_that("pkgs: concat two pkgs", {
  ps1 <- pkgs("dplyr", "test")
  ps2 <- pkgs("dose/nmrunner")
  c_ps <- c(ps1, ps2)
  expect_s3_class(c_ps, "pkgs")
  expect_length(c_ps, 3)
  expect_identical(c_ps[1:2], ps1)
  expect_identical(c_ps[3], ps2)
})
