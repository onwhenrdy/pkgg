.concat_pkgs <- function(...) {
  args <- list(...)
  if (length(args) == 1) {
    return(args[[1]])
  }

  res <- purrr::map(args, \(x) {
    if (is(x, "pkg")) {
      x
    } else if (is(x, "pkgs")) {
      x |> purrr::map(identity)
    } else {
      cli::cli_abort(
        c(
          "i" = "All arguments must be of class {.cls pkg} or {.cls pkgs}.",
          "x" = "You supplied an argument of class {.cls { class(x)}}"
        ),
        call = rlang::caller_call(4)
      )
    }
  }) |>
    purrr::list_flatten()
  res <- purrr::set_names(res, purrr::map_chr(res, purrr::pluck("package")))

  class(res) <- "pkgs"
  return(res)
}


#' @method c pkg
#' @export
c.pkg <- function(...) {
  .concat_pkgs(...)
}

#' @method c pkgs
#' @export
c.pkgs <- function(...) {
  .concat_pkgs(...)
}

#' @export
as_pkgs <- function(...) {
  args <- list(...)
  if (length(args) == 1 && is(args[[1]], "pkg")) {
    args[[1]] <- list(args[[1]])
    class(args[[1]]) <- "pkgs"
  }
  do.call(.concat_pkgs, args)
}

#' @method [ pkgs
#' @export
`[.pkgs` <- function(x, i, ...) {
  class(x) <- NULL
  res <- base::`[`(x, i, ...)
  class(res) <- "pkgs"
  return(res)
}
