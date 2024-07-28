test_that("standard pck can be set", {
  p <- pkg("dplyr")
  expect_equal(p$package, "dplyr")
  expect_equal(p$ref, "dplyr")
  expect_equal(p$version, NA)
})

test_that("pkg can be set with version", {
  p <- pkg("dplyr@0.8.0")
  expect_equal(p$package, "dplyr")
  expect_equal(p$ref, "dplyr@0.8.0")
  expect_equal(p$version, "==0.8.0")
})

test_that("pkg can be set with name", {
  p <- pkg("url::http://dplyr.tar.gz", "dplyr")
  expect_equal(p$package, "dplyr")
})

test_that("pkg fails if name is needed", {
  expect_error(pkg("url::http://dplyr.tar.gz"))
})

test_that("pkg fails if name is invalid", {
  expect_error(pkg("url::http://dplyr.tar.gz", "dplyr-"))
})

test_that("pkg warns if name can be inferred but is provided", {
  expect_warning(pkg("dplyr", pkg_name = "dplyr"))
})

test_that("pkg fails if non-valid version was provided", {
  expect_error(pkg("dplyr@>2.0"))
})

test_that("pkg short versions work", {
  p_local <- pkg_local("dplyr@0.8.0")
  expect_equal(p_local$install, "local")

  p_container <- pkg_container("dplyr@0.8.0")
  expect_equal(p_container$install, "container")
})

test_that("pkg does not fail or warn if printed", {
  p <- pkg("dplyr")
  expect_output(print(p), "dplyr")
})
