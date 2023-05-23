#' Data Filtering Function
#'
#' This function filters a dataset based on input specifications.
#'
#' @param data A data.frame that will be filtered.
#' @param dic A dictionary data.frame containing the 'id' of variable names and their data types.
#' @param var_inputs A list of variables to filter, where names are variable names and values are the desired values or ranges.
#' @param special_placeholder (Optional) A placeholder value to indicate special conditions.
#' @param .id (Optional) A special identifier for dealing with list variables.
#'
#' @return A filtered data.frame.
#' @export
data_filter <- function(data,
                        dic,
                        var_inputs,
                        special_placeholder = NULL,
                        .id = NULL) {

  if (is.null(data) || is.null(dic)) return()
  if (is.null(var_inputs) || !is.list(var_inputs)) return(data)


  df <- purrr::reduce(seq_along(var_inputs), function(df, .x) {
    if (!is.null(var_inputs[[.x]]) && !setequal(var_inputs[[.x]], "")) {
      other_condition <- !is.null(special_placeholder) && setequal(var_inputs[[.x]], special_placeholder)
      if (!other_condition) {
        name_var <- names(var_inputs)[.x]
        info_var <- dic |> dplyr::filter(id %in% name_var)
        filter_var <- var_inputs[[.x]]
        if (info_var$hdtype == "Dat" || info_var$hdtype == "Num") {
          df <- filter_ranges(df, range = filter_var, by = info_var$id)
        }
        if (info_var$hdtype == "list") {
          df <- filter_list(df, filter_var, info_var$id, .id = .id)
        }
        if (info_var$hdtype == "Cat") {
          df <- df |> dplyr::filter(!!dplyr::sym(info_var$id) %in% filter_var)
        }
      }
    }
    df
  }, .init = data)

  df
}
