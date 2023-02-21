has_all_na_rows <- function(d, cols){
  if(is.null(cols)) cols <- names(d)
  d <- d[,cols]
  any(apply(d, 1, function(d) all(is.na(d))))
}

which_all_na_rows <- function (d, cols = NULL, na = c(NA)) {
  if(is.null(cols)) cols <- names(d)
  d <- d[,cols]
  i <- 1:nrow(d)
  idx <- apply(d, 1, function(d) all(is.na(d)))
  i[idx]
}
