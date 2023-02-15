aggregation <- function(aggregation_function, ..., na.rm = TRUE) {
  if (!is.function(aggregation_function)) {
    stop("aggregation_function must be a function")
  }
  if (is.null(aggregation_fun) || is.na(aggregation_fun)) {
    return()
  } else {
    do.call(aggregation_fun, c(list(...), na.rm = na_rm))
  }
}

aggregation_data <- function (data, agg, group_var, to_agg, name = NULL) {
  if (is.null(data)) return()
  if (agg == "count") {
    result <- data %>%
      dplyr::group_by(across(dplyr::all_of(group_var))) %>%
      dplyr::summarise(count = dplyr::n())
    if (!is.null(name)) {
      names(result)[ncol(result)] <- name
    }
  } else {
    result <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) %>%
      dplyr::summarise(across(dplyr::all_of(to_agg),
                              ~ aggregation(agg, .x), .names = name))
  }
  result
}
