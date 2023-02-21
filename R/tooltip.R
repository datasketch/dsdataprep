
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
