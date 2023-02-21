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
#'
#' @return A character vector of tooltip strings, or a data frame with a new column of tooltip strings.
#'
#' @export
prep_tooltip <- function(data, tooltip = NULL, new_labels = NULL,
                         engine = "html", as_df = FALSE,
                         na_row_default_column = NULL,
                         na_row_default_value = NULL,
                         na_label = "(NA)"
                         ){

  if(is.null(tooltip)){
    d <- data
    if(!is.null(na_row_default_column)){
      d[[na_row_default_column]] <- NULL
    }
    tooltip <- create_default_tpl(d, new_labels = new_labels,
                                  engine = engine)
  }
  # Make sure all variables in template are in the data
  used_vars <- get_tpl_vars(tooltip)
  check_tpl_vars(data, tooltip)

  if(!has_all_na_rows(data, cols = used_vars)){
    v <- glue::glue_data(data, tooltip, .na = na_label)
  }else{
    all_nas_idx <- which_all_na_rows(data, cols = used_vars)
    if(!is.null(na_row_default_column)){
      defaults <- data[[na_row_default_column]]
    } else {
      defaults <- rep(na_row_default_value %||% na_label, nrow(data))
    }
    v <- glue::glue_data(data, tooltip, .na = na_label)
    v[all_nas_idx] <- defaults[all_nas_idx]
  }

  if(as_df){
    data$..tooltip <- as.character(v)
    return(data)
  }
  as.character(v)
}

create_default_tpl <- function(data, new_labels = NULL,
                               engine = "html"){

  if(is.null(new_labels)){
    new_labels <- names(data)
  }

  vars <- names(data)
  names(vars) <- new_labels

  collapse_symbol <- case_when(
    engine == "html" ~ "<br/>",
    engine == "markdown" ~ "\n\n",
    .default = "\n"
  )

  f_html <- function(x, nm){
    paste0("<b>",nm, ":</b> {", x, "}")
  }
  f_markdown <- function(x, nm){
    paste0("**",nm, ":** {", x, "}")
  }
  f_txt <- function(x, nm){
    paste0("",nm, ": {", x, "}")
  }

  fs <- list(
    html = f_html,
    markdown = f_markdown,
    txt = f_txt
  )
  render_fun <-  fs[[engine]]
  if(is.null(render_fun)) render_fun <- f_txt

  tooltip <- purrr::imap(as.list(vars), ~ render_fun(.x, .y)) |>
    unlist() |>
    paste(collapse = collapse_symbol)
  tooltip

}


check_tpl_vars <- function(data, tooltip){
  vars <- get_tpl_vars(tooltip)
  if(!all(vars %in% names(data)))
     stop("Not all variables in template are found in the data")
}


get_tpl_vars <- function(tpl){
  unlist(stringr::str_extract_all(tpl, "(?<=\\{).+?(?=\\})"))
}
