# es una funcion que es ejecutada por do.call puede ser sum, mean, max, min, sd, etc
#' @keywords internal
aggregation <- function(aggregation, ...) {
  if (is.null(aggregation) || is.na(aggregation)) {
    stop("The aggregation type must be specified.")
  }

  result <- do.call(aggregation, c(list(...), list(na.rm = TRUE)))

  if (is.na(result)) {
    stop("The aggregation failed.")
  }

  return(result)
}


#' @keywords internal
paste_vector <- function(x, collapse = ",") {
  paste0(trimws(unique(x)), collapse = collapse)
}
