.package_name_rexp <- function() "^(?!.*\\s)[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]$"

#' @export
pkg <- function(
    ref,
    pkg_name,
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
    if (missing(pkg_name)) {
      cli::cli_abort(
        c(
          "The package name could not be inferred from the reference.",
          "x" = "The refference was {.field {ref}}.",
          "i" = "You must provide the {.var pkg_name} argument.",
          "i" = "See {.code ?pkgdepends::pkg_refs()}` for more information."
        )
      )
    }

    checkmate::assertString(pkg_name, min.chars = 1)
    if (!grepl(.package_name_rexp(), pkg_name, perl = TRUE)) {
      cli::cli_abort(
        c("The package name must be a valid R package name.",
          "x" = "The failty package name was {.field {pkg_name}}.",
          "i" = "See {.code ?pkgdepends::pkg_refs()} for more information."
        )
      )
    }

    res$package <- pkg_name
  } else {
    if (!missing(pkg_name)) {
      cli::cli_warn(
        c("{.var pkg_name} is ignored for reference {.field {ref}}.",
          "i" = "The package name could be inferred from the reference."
        )
      )
    }
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
pkg_local <- function(ref, pkg_name, .load = TRUE) {
  pkg(ref, pkg_name, .install = "local", .load = .load)
}

#' @export
pkg_container <- function(ref, pkg_name, .load = TRUE) {
  pkg(ref, pkg_name, .install = "container", .load = .load)
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











install_packages <- function(pkgs, upgrade = TRUE, .exit_on_error = FALSE, .cleanup = TRUE) {
  if (length(pkgs) == 0) {
    return(invisible(TRUE))
  }

  tryCatch(
    {
      pkg_urls <- pkgs |> purrr::map_chr(x$pkg)
      pak::pkg_install(pkg_urls, upgrade = upgrade)
    },
    error = function(e) {
      if (.exit_on_error) {
        message("Error installing packages: ", e$message)
        q(status = 1)
      } else {
        stop(e, call. = FALSE)
      }
    }
  )

  if (.cleanup) {
    pak::pak_cleanup(package_cache = TRUE, metadata_cache = TRUE, force = TRUE)
  }

  invisible(TRUE)
}
