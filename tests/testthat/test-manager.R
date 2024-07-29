.unload <- function(...) {
  x <- list(...)
  purrr::walk(x, \(p) try(unloadNamespace(p), silent = TRUE))
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


test_that("local packages can be installed and not cleanup runs", {
  tmp_dir <- tempdir()
  tmp <- tempfile(pattern = "file", tmpdir = tmp_dir, fileext = "")
  dir.create(tmp)

  test_pkg_path <- file.path(test_path(), "testdata", "testpkg")
  p1 <- paste0("testpkg=local::", test_pkg_path)
  p2 <- "car"
  withr::defer(.unload(p1, p2))
  withr::defer(unlink(tmp, recursive = TRUE))

  fn <- pkg_manager(
    pkg_local(p1),
    pkg_container(p2)
  )

  fn("install_local", lib = tmp_dir) |> capture.output()
  expect_no_error(
    library("testpkg", lib.loc = tmp_dir, character.only = TRUE) |>
      suppressWarnings()
  )

  expect_error(library(p2, lib.loc = tmp_dir, character.only = TRUE))
  expect_true(pak::cache_list() |> nrow() > 0)
})

test_that("container packages can be installed and cleanup runs", {
  tmp_dir <- tempdir()
  tmp <- tempfile(pattern = "file", tmpdir = tmp_dir, fileext = "")
  dir.create(tmp)

  test_pkg_path <- file.path(test_path(), "testdata", "testpkg")
  p1 <- paste0("testpkg=local::", test_pkg_path)
  p2 <- "car"
  withr::defer(.unload(p1, p2))
  withr::defer(unlink(tmp, recursive = TRUE))

  fn <- pkg_manager(
    pkg_local(p2),
    pkg_container(p1)
  )

  fn("install_container", lib = tmp_dir) |> capture.output()
  expect_no_error(
    library("testpkg", lib.loc = tmp_dir, character.only = TRUE) |>
      suppressWarnings()
  )

  expect_error(library(p2, lib.loc = tmp_dir, character.only = TRUE))
  expect_true(pak::cache_list() |> nrow() == 0)
})

test_that("Install fail will re-throw", {
  withr::with_tempdir({
    fn <- pkg_manager(pkg_local("nopkg=./nopkg.tag.gz"))
    expect_error(fn("install_local", lib = getwd()) |> capture.output())
  })
})

test_that("Install fail will re-throw", {
  withr::with_tempdir({
    local_mocked_bindings(
      q = function(...) stop("mocked error")
    )

    fn <- pkg_manager(pkg_container("nopkg=./nopkg.tag.gz"))
    expect_error(fn("install_container", lib = getwd()) |> capture.output(), "mocked error")
  })
})
