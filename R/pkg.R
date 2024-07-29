#' @export
pkg <- function(
    ref,
    .install = c("both", "local", "container"),
    .load = TRUE) {
  checkmate::assertString(ref, min.chars = 1)
  checkmate::assert_flag(.load)
  .install <- rlang::arg_match(.install)

  pkg_infos <- pkgdepends::parse_pkg_ref(ref)
  res <- list(
    package = pkg_infos$package,
    ref = pkg_infos$ref,
    type = pkg_infos$type,
    version = NA,
    install = .install,
    load = .load
  )

  if (is.na(res$package)) {
    cli::cli_abort(
      c(
        "The package name could not be inferred from the reference.",
        "i" = "You must provide the package name via {.code name={ref}}.",
        "i" = "See {.code ?pkgdepends::pkg_refs()}` for more information."
      )
    )
  }

  # if version info is accepted by pkgdepends
  if (!is.null(pkg_infos$version) && pkg_infos$version != "") {
    res$version <- paste0(pkg_infos$atleast, pkg_infos$version)
  }

  res <- structure(res)
  class(res) <- "pkg"
  return(res)
}

#' @export
pkg_local <- function(ref, .load = TRUE) {
  pkg(ref, .install = "local", .load = .load)
}

#' @export
pkg_container <- function(ref, .load = TRUE) {
  pkg(ref, .install = "container", .load = .load)
}

#' @export
#' @method format pkg
format.pkg <- function(x, detailed = TRUE, ...) {
  checkmate::assert_flag(detailed)

  load_msg <- if (x$load) cli::col_green("yes") else cli::col_red("no")
  install_msg <- switch(x$install,
    local = "locally",
    container = "container",
    "locally and container"
  )

  header <- paste0(x$package, if (is.na(x$version)) "" else paste0(" [", x$version, "]"))

  add <- NULL
  if (detailed) {
    add <- c(
      "i" = "Source : {.field {x$ref}} from {.field {x$type}}",
      "i" = "Install: {.field {install_msg}}",
      "i" = "Loading: {load_msg}"
    )
  }

  cli::cli_format_method({
    cli::cli_bullets(c(">" = "{cli::col_red({header})}", add))
  })
}

#' @export
#' @method print pkg
print.pkg <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}
