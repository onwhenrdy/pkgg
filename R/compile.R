.load_pkgs <- function(pkgs, show_startup_msg, warnings, ...) {
  refs <- purrr::keep(pkgs, \(x) x$load) |>
    purrr::map(\(x) x$package)

  warn_fn <- if (warnings) function(x) {} else suppressWarnings
  msg_fn <- if (show_startup_msg) function(x) {} else suppressPackageStartupMessages
  msg_fn_2 <- if (show_startup_msg) function(x) {} else suppressMessages

  purrr::walk(refs, \(ref) {
    library(ref, character.only = TRUE, quietly = TRUE, ...) |>
      warn_fn() |>
      msg_fn() |>
      msg_fn_2()
  })
}

.install_pkgs <- function(pkgs, where, exit_on_error, ...) {
  inv_target <- if (where == "local") "container" else "local"

  refs <- purrr::keep(pkgs, \(x) x$install != inv_target) |>
    purrr::map_chr(\(x) x$ref)

  if (length(refs) == 0) {
    return(invisible())
  }

  exit_if_error <- switch(exit_on_error,
    container = where == "container",
    local = where == "local",
    both = TRUE,
    none = FALSE
  )

  tryCatch(pak::pkg_install(refs, upgrade = TRUE, ...),
    error = function(e) {
      if (exit_if_error) {
        q(status = 1)
      }

      cli::cli_abort("Installing packages failed", parent = e)
    }
  )

  return(invisible())
}

.show_list <- function(startup_msg, warnings, error_on_exit) {
  cli::cli_format_method({
    cli::cli_h2("Usage")
    cli::cli_text("{.code fn(action = 'load_pkgs', startup_msg = TRUE, warnings = TRUE)}")
    cli::cli_h2("Action")
    cli::cli_ul(c(
      "*" = "{.strong list}: Print packge definitions as table",
      "*" = "{.strong load_pkgs}: Load packged defined with {.field load}",
      "*" = "{.strong install_local}: Install packages for {.field local installation}",
      "*" = "{.strong install_container}: Install packages for {.field container}"
    ))
    cli::cli_h2("Arguments")
    cli::cli_ul(c(
      "*" = "{.strong startup_msg}: Show startup messages for packages (default: {.field {startup_msg}})",
      "*" = "{.strong warnings}: Show warnings on package loading (default: {.field {warnings}})"
    ))
    cli::cli_text("")
    cli::cli_text(cli::col_red("Exit R with error code for: {error_on_exit}"))
    cli::cli_text("")
  }) |> cat(sep = "\n")
}

# #' @export
pkg_manager <- function(
    ...,
    .exit_on_error = c("container", "local", "both", "none"),
    .startup_msg = FALSE,
    .warnings = FALSE) {
  pkgs <- list(...)

  for (pkg in pkgs) {
    checkmate::assert(
      checkmate::check_class(pkg, "pkg"),
      checkmate::check_class(pkg, "pkgs")
    )
  }


  .exit_on_error <- rlang::arg_match(.exit_on_error)


  res <- function(action = c(
                    "list",
                    "load_pkgs",
                    "install_local",
                    "install_container"
                  ),
                  startup_msg = .startup_msg,
                  warnings = .warnings,
                  ...) {
    checkmate::assert_flag(startup_msg)
    checkmate::assert_flag(warnings)

    show_list <- missing(action)
    action <- rlang::arg_match(action)

    exit_on_error <- .exit_on_error

    if (show_list) {
      .show_list(startup_msg, warnings, exit_on_error)
      return(invisible())
    }

    packages <- do.call(c, pkgs)
    if (action == "load_pkgs") {
      .load_pkgs(packages, startup_msg, warnings, ...)
    } else if (action == "install_local") {
      .install_pkgs(packages, "local", exit_on_error, ...)
    } else if (action == "install_container") {
      .install_pkgs(packages, "container", exit_on_error, ...)
    } else {
      print(packages, table = TRUE)
    }
  }

  return(res)
}
