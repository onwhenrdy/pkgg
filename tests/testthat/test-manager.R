.unload <- function(...) {
  x <- list(...)
  purrr::walk(x, \(p) try(unloadNamespace(p), silent = TRUE))
}

.test_pkgs <- function() {
  list(
    list(
      name = "testpkg1",
      path = paste0("testpkg1=local::", file.path(test_path(), "testdata", "testpkg1"))
    ),
    list(
      name = "testpkg2",
      path = paste0("testpkg2=local::", file.path(test_path(), "testdata", "testpkg2"))
    )
  )
}

# ------------------------------------------------------------------------------
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

# setup 2 packages (1 local and 1 container)
.setup_install_tests <- function(inject_fn) {
  test_pkgs <- .test_pkgs()
  p1_name <- test_pkgs[[1]]$name
  p2_name <- test_pkgs[[2]]$name

  .unload(p1_name, p2_name)
  withr::defer(.unload(p1_name, p2_name))

  tmp_dir <- file.path(tempdir(), "pkggtest")
  dir.create(tmp_dir, showWarnings = FALSE)
  withr::defer({
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
  })

  fn <- pkg_manager(
    pkg_local(test_pkgs[[1]]$path),
    pkg_container(test_pkgs[[2]]$path, .load = FALSE)
  )

  inject_fn(tmp_dir, fn, p1_name, p2_name)
}

test_that("install and loading for local pkgs works", {
  inject_fn <- function(tmp_dir, fn, p1_name, p2_name) {
    withr::with_envvar(c("R_USER_CACHE_DIR" = tmp_dir), {
      expect_no_error(fn("install_local", lib = tmp_dir) |> capture.output())
      expect_no_error(fn("load_pkgs", lib.loc = tmp_dir))

      # only local package should be loaded
      expect_true(p1_name %in% loadedNamespaces())
      expect_false(p2_name %in% loadedNamespaces())
      # Cache should be populated
      expect_true(pak::cache_list() |> nrow() > 0)
      .unload(p1_name, p2_name)
    })
  }

  .setup_install_tests(inject_fn)
})

test_that("install and loading for local and container pkgs works", {
  inject_fn <- function(tmp_dir, fn, p1_name, p2_name) {
    withr::with_envvar(c("R_USER_CACHE_DIR" = tmp_dir), {
      expect_no_error(fn("install_container", lib = tmp_dir) |> capture.output())

      # since we load a local package that is not installed
      expect_error(fn("load_pkgs", lib.loc = tmp_dir))

      # only local package should be loaded
      expect_false(p1_name %in% loadedNamespaces())
      expect_false(p2_name %in% loadedNamespaces())
      # Cache should be not be populated
      expect_true(pak::cache_list() |> nrow() == 0)
    })
  }

  .setup_install_tests(inject_fn)
})


test_that("Install fail will re-throw", {
  withr::with_tempdir({
    fn <- pkg_manager(pkg_local("nopkg=./nopkg.tag.gz"))
    expect_error(fn("install_local", lib = getwd()) |> capture.output())
  })
})

test_that("Install fail will exit", {
  withr::with_tempdir({
    local_mocked_bindings(
      q = function(...) stop("mocked error")
    )

    fn <- pkg_manager(pkg_container("nopkg=./nopkg.tag.gz"))
    expect_error(fn("install_container", lib = getwd()) |> capture.output(), "mocked error")
  })
})
