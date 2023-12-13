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



create_default_tpl <- function(data_names, new_labels = NULL,
                               engine = "html"){

  if(is.null(new_labels)){
    new_labels <- data_names
  }

  vars <- data_names
  names(vars) <- new_labels

  collapse_symbol <- dplyr::case_when(
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

  tooltip <- purrr::imap(as.list(vars), ~ render_fun(.x, gsub("\\.\\.", " ",.y))) |>
    unlist() |>
    paste(collapse = collapse_symbol)

  tooltip

}


check_tpl_vars <- function(data_names, tooltip){
  vars <- get_tpl_vars(tooltip)
  if(!all(vars %in% data_names))
    stop("Not all variables in template are found in the data")
}


get_tpl_vars <- function(tpl){
  unlist(stringr::str_extract_all(tpl, "(?<=\\{).+?(?=\\})"))
}


filter_ranges <- function(data, range, by) {
  if (is.null(data)) return()
  if (is.null(range)) return(data)

  column_values <- data[[by]]
  if (all(is.na(column_values))) return(data)
  min_value <- min(column_values, na.rm = TRUE)
  max_value <- max(column_values, na.rm = TRUE)

  if (all(is.na(column_values))) {
    return(data)
  } else if (length(range) == 2) {
    if (!is.finite(min_value) || !is.finite(max_value)) {
      return(data)
    } else if (min_value == range[1] & max_value == range[2]) {
      return(data)
    } else {
      data_filter <- dplyr::filter(data, !!dplyr::sym(by) >= range[1], !!dplyr::sym(by) <= range[2])
    }
  } else {
    data_filter <- dplyr::filter(data, !!dplyr::sym(by) == range)
  }
  data_filter
}



paste_vector <- function(x, collapse = ",") {
  paste0(trimws(unique(x)), collapse = collapse)
}

# filter variables with several categories in one row
filter_list <- function(data, cats, by, .id) {
  if (is.null(data)) return()
  if (is.null(cats)) return()
  if (is.null(by)) return()

  temporal_df <- data[,c(.id, by)] |>
    tidyr::separate_rows({{ by }}, sep = ",") |>
    dplyr::filter(!!dplyr::sym(by) %in% cats) |>
    dplyr::group_by(!!dplyr::sym(.id)) |>
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   list(paste_vector), .names = by))
  data <- data[, -grep(by, names(data))]
  data <- data |> dplyr::inner_join(temporal_df)
  data
}


