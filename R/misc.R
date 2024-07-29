is <- function(obj, cls) {
  if (is.null(obj)) return(FALSE)
  return(cls %in% class(obj))
}