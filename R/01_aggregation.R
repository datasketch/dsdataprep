#' Aggregate data by group and perform summary operations
#'
#' This function aggregates and summaries data. Supported summary operations
#' includes count, sum, mean, and more.
#'
#' @param data A data frame
#' @param agg The type of aggregation to perform (e.g., "count", "sum", "mean")
#' @param group_var A character vector of variable names to group by
#' @param to_agg A character vector of variable names to summarize
#' @param agg_name A character vector of new names for the summary variables
#' @param percentage A logical value indicating whether to calculate percentages
#' @param percentage_name A character vector of new names for percentage columns
#' @param percentage_col A character with the name of the variable for which you want the percentage to be calculated.
#' @param extra_col A logical value indicating whether to include an additional column in the output data frame containing the concatenated values of any factor or character columns.
#' @param agg_extra A character string indicating the aggregation function to use for the additional column if \code{extra_col} is \code{TRUE}. Options include "sum", "mean", "median", "min", "max", "count", and "n_distinct".
#'
#' @return A data frame containing the aggregated data.
#'
#' @import dplyr
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' data(mtcars)
#' aggregation_data(data = mtcars,
#'                  agg = "mean",
#'                  group_var = c("cyl", "gear"),
#'                  to_agg = c("mpg", "disp"))
#'
#' @seealso
#' \code{\link{summarize_all}}, \code{\link{group_by}}, \code{\link{across}}
#'
#' @family dplyr functions

aggregation_data <- function (data,
                              agg,
                              group_var,
                              to_agg,
                              agg_name = NULL,
                              na_rm = TRUE,
                              na_label = "(NA)",
                              percentage = FALSE,
                              percentage_name = NULL,
                              percentage_col = NULL,
                              extra_col = FALSE,
                              agg_extra = "sum",
                              extra_sep = ",",
                              extra_group = NULL,
                              collapse_columns = NULL,
                              numeric_collapse_columns = NULL,
                              extra_sep_collapse_columns = "-") {

  if (is.null(data)) stop("The data object must be specified")
  if (is.null(group_var)) stop("The pooling variable(s) must be specified.")

  class_data <- class(data)
  ..percentage <- NULL
  if ("fringe" %in% class_data) data <- data$data

  if (!is.null(na_label)) {
     data <- data |>
       dplyr::mutate_at(group_var, ~tidyr::replace_na(.,na_label))
  }

  if (extra_col) {
    gv <- group_var
    if (!is.null(extra_group)) {
      gv <- c(gv, extra_group)
      if (!is.null(collapse_columns)) {
        cols <- c(collapse_columns, "..num_add")

        extra_collapse <- data |>
          group_by(across(all_of(gv))) |>
          summarise(across(all_of(numeric_collapse_columns), ~ aggregation(agg_extra, na_rm = na_rm, .x), .names = "..num_add"))

        extra_collapse <- extra_collapse |>
          tidyr::unite("..collapse", {cols}, sep = extra_sep_collapse_columns, remove = F)
        data <- data |> left_join(extra_collapse, by = gv)
      }
    }
    extra_data <- data |>
      group_by(across(all_of(group_var))) |>
      summarise(across(where(~ is.numeric(.x) | is.integer(.x)), ~ aggregation(agg_extra, na_rm = na_rm, .x), .names = "{.col}_extra"),
                across(where(~ is.factor(.x) | is.character(.x)), ~ paste_vector(.x, extra_sep), .names = "{.col}"))

  }


  if (agg == "count") {
    result <- data |>
      group_by(across(all_of(group_var))) |>
      summarise(count = n())
    if (percentage) {
      result <- result |>
        mutate(..percentage = (count / sum(count))*100)
      if (!is.null(percentage_name)) {
        result <- result |> rename(!!percentage_name := ..percentage)
      }
    }
    if (!is.null(agg_name)) {
      result <- result |> rename(!!agg_name := count)
    }
  } else {
    result <- data |>
      group_by(across(all_of(group_var))) |>
      summarise(across(all_of(to_agg),
                       ~ aggregation(agg, na_rm = na_rm, as.numeric(.x)),
                       .names = ifelse(is.null(agg_name), "{.col}", "{agg_name}")))

    if (percentage) {
      to_percentage <- to_agg
      if (!is.null(agg_name)) to_percentage <- agg_name
      if (is.null(percentage_name)) percentage_name <- paste0("..percentage ", agg_name)
      if (!is.null(percentage_col)) {
        result <- result |>
          group_by(across(all_of(percentage_col)))
      }
      result <- result |>
        mutate(across(all_of(to_percentage), ~ . / sum(., na.rm = TRUE) * 100,
                      .names = "{percentage_name}"))
    }

  }



  if (extra_col) {
    result <- result |> left_join(extra_data)
  }



  result
}
