.unload <- function(...) {
  x <- list(...)
  purrr::walk(x, unloadNamespace)
}

test_that("menu is shown if no action is defined", {
  fn <- pkg_manager(
    pkgs()
  )
  expect_output(fn(), "Usage")
})

test_that("packages are listed for option `list`", {
  fn <- pkg_manager(
    pkg("nlme"),
    pkgs("MASS")
  )
  expect_output(fn("list"), "nlme")
  expect_output(fn("list"), "MASS")
})

test_that("Packages are (not) loaded as defined", {
  # these should be save to unload in the test and we can expect that
  # they are installed as part of vase R installations
  p1 <- "nlme"
  p2 <- "MASS"
  .unload(p1, p2)
  withr::defer(.unload(p1, p2))

  fn <- pkg_manager(
    pkg(p1),
    pkgs(p2, .load = FALSE)
  )
  fn("load_pkgs")

  expect_true(p1 %in% loadedNamespaces())
  expect_false(p2 %in% loadedNamespaces())
})

tryCatch(
  cli::cli_abort(
    c("BLA",
      "AA",
      "i" = "All arguments must be of class {.cls pkg} or {.cls pkgs}."
    )
  ),
  error = function(e) {
    cli::cli_abort("Test", parent = e)
  }
)
