#' Prepare tooltips for data in a table or plot
#'
#' This function generates a tooltip string for each row of a data frame or matrix. Tooltips are typically used to display additional information about the data when the user hovers over a data point in a plot or table.
#'
#' @param data A data frame or matrix containing the data to display in the tooltip.
#' @param tooltip An optional tooltip template string that specifies how to format the data for display. If not specified, a default template will be created based on the column names of the data frame.
#' @param new_labels An optional named character vector of column labels to use in the tooltip. If specified, the column labels in the tooltip will be replaced with the new labels.
#' @param engine A character string specifying the templating engine to use. Currently supported options include "html", "markdown", and "latex".
#' @param as_df A logical value indicating whether to return the tooltip strings as a new column in the input data frame. If \code{FALSE} (the default), the tooltip strings will be returned as a character vector.
#' @param na_row_default_column An optional character string specifying the name of a column to use as the default value when all other columns in a row are \code{NA}. If specified, the values in this column will be used as the default tooltip value. If not specified, \code{na_row_default_value} will be used instead.
#' @param na_row_default_value An optional character string to use as the default tooltip value when all columns in a row are \code{NA}. If \code{NULL} (the default), the string "(NA)" will be used.
#' @param na_label An optional character string to use for \code{NA} values in the tooltip. If not specified, the string "(NA)" will be used.
#' @param format_num A named list of arguments to pass to the \code{\link{makeup_num}} function.
#' @param opts_format_num A named list of options to use for formatting numeric data.
#' @param format_cat A named list of arguments to pass to the \code{\link{makeup_chr}} function.
#' @param format_date A named list of arguments to pass to the \code{\link{makeup_dat}} function.
#'
#' @return A character vector of tooltip strings, or a data frame with a new column of tooltip strings.
#'
#' @export
prep_tooltip <- function(data, tooltip = NULL, new_labels = NULL,
                         engine = "html", as_df = FALSE,
                         na_row_default_column = NULL,
                         na_row_default_value = NULL,
                         na_label = "(NA)",
                         format_num = NULL,
                         opts_format_num = NULL,
                         format_cat = NULL,
                         format_date = NULL
){


  if(is.null(tooltip)){
    d <- data
    if(!is.null(na_row_default_column)){
      d[[na_row_default_column]] <- NULL
    }
    tooltip <- create_default_tpl(names(d), new_labels = new_labels,
                                  engine = engine)
  }
  # Make sure all variables in template are in the data
  used_vars <- get_tpl_vars(tooltip)

  check_tpl_vars(names(data), tooltip)
  data <- data[, c(used_vars, na_row_default_column)]

  if (!is.null(format_num)) {
    data <- data |>
      mutate(across(where(~ is.numeric(.x) | is.integer(.x)),
                    ~ makeup::makeup_num(.x,
                                         sample = format_num,
                                         prefix = opts_format_num$prefix %||% "",
                                         suffix = opts_format_num$suffix %||% "",
                                         si_prefix = opts_format_num$si_prefix %||% FALSE),
                    .names = "{.col}"))
  }
  if (!is.null(format_cat)) {
    data <- data |>
      mutate(across(where(~ is.factor(.x) | is.character(.x)),
                    ~ makeup::makeup_chr(.x, sample = format_cat),
                    .names = "{.col}"))
  }
  if (!is.null(format_date)) {
    data <- data |>
      mutate(across(where(~ lubridate::is.Date(.x)),
                    ~ makeup::makeup_dat(.x, sample = format_cat),
                    .names = "{.col}"))
  }


  if (!has_all_na_rows(data, cols = used_vars)){
    v <- glue::glue_data(data, tooltip, .na = na_label)
  } else {
    all_nas_idx <- which_all_na_rows(data, cols = used_vars)
    if (!is.null(na_row_default_column)){
      defaults <- data[[na_row_default_column]]
    } else {
      defaults <- rep(na_row_default_value %||% na_label, nrow(data))
    }
    v <- glue::glue_data(data, tooltip, .na = na_label)
    v[all_nas_idx] <- defaults[all_nas_idx]
  }

  if (as_df) {
    data$..tooltip <- as.character(v)
    return(data)
  }
  as.character(v)
}
