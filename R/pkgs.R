#' @export
pkgs <- function(..., .install = c("both", "local", "container"), .load = TRUE) {
  checkmate::assert_flag(.load)
  .install <- rlang::arg_match(.install)

  args <- list(...)
  res <- purrr::map(args, \(x) {
    if (is.character(x)) {
      purrr::map(x, pkg, .install = .install, .load = .load)
    } else {
      cli::cli_abort(
        c(
          "All arguments in {.var ...} must be of class {.cls character}.",
          "x" = "You supplied an argument of class {.cls {class(x)}}."
        ),
        call = rlang::caller_call(3)
      )
    }
  }) |>
    purrr::list_flatten()

  res <- purrr::set_names(res, purrr::map_chr(res, purrr::pluck("package")))

  class(res) <- "pkgs"
  return(res)
}

#' @export
pkgs_local <- function(..., .load = TRUE) {
  pkgs(..., .install = "local", .load = .load)
}

#' @export
pkgs_container <- function(..., .load = TRUE) {
  pkgs(..., .install = "container", .load = .load)
}

#' @method as.data.frame pkgs
#' @export
as.data.frame.pkgs <- function(x, ...) {
  if (length(x) == 0) {
    return(data.frame(
      package = character(), type = character(),
      reference = character(), version = character(),
      install = character(), load = logical()
    ))
  }

  data.frame(
    package = purrr::map_chr(x, purrr::pluck("package")),
    type = purrr::map_chr(x, purrr::pluck("type")),
    reference = purrr::map_chr(x, purrr::pluck("ref")),
    version = purrr::map_chr(x, purrr::pluck("version")),
    install = purrr::map_chr(x, purrr::pluck("install")),
    load = purrr::map_lgl(x, purrr::pluck("load"))
  )
}

#' @method format pkgs
#' @export
format.pkgs <- function(x, table = FALSE, detailed = FALSE, ...) {
  checkmate::assert_flag(table)
  checkmate::assert_flag(detailed)

  if (table) {
    return(as.data.frame(x))
  }

  if (length(x) == 0) {
    return(cli::col_red("No packages added\n"))
  }

  n <- length(x)
  n_loaded <- sum(purrr::map_lgl(x, purrr::pluck("load")))
  n_container <- sum(purrr::map_lgl(x, \(x) x$install != "local"))
  n_local <- sum(purrr::map_lgl(x, \(x) x$install != "container"))

  cli::cli_format_method({
    cli::cli_h1("{n} Package{?s}")
    cli::cli_text("")
    purrr::walk(x, print, detailed, ...)
    cli::cli_h1(paste0(
      "Loads: {n_loaded}/{n} |",
      " Installs: {n_container}/{n} container / {n_local}/{n} locally"
    ))
  })
}

#' @method print pkgs
#' @export
print.pkgs <- function(x, table = FALSE, detailed = FALSE, ...) {
  checkmate::assert_flag(table)
  checkmate::assert_flag(detailed)

  if (table) {
    print(format(x, table = TRUE, ...))
  } else {
    cat(format(x, detailed = detailed, ...), sep = "\n")
  }
}
