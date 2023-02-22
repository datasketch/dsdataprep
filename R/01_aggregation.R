#' Aggregate data by group and perform various summary operations
#'
#' This function aggregates data by a specified grouping variable or variables
#' and calculates summary statistics on one or more other variables in the data.
#' Supported summary operations include count, sum, mean, and more.
#'
#' @param data A data frame to be summarized
#' @param agg The type of aggregation to perform (e.g., "count", "sum", "mean")
#' @param group_var A character vector of variable names to group by
#' @param to_agg A character vector of variable names to summarize
#' @param agg_name A character vector of new names for the summary variables
#' @param percentage A logical value indicating whether to calculate percentages
#' @param percentage_name A character vector of new names for percentage columns
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
#' aggregation_data(mtcars, "mean", c("cyl", "gear"), c("mpg", "disp"))
#'
#' @seealso
#' \code{\link{summarize_all}}, \code{\link{group_by}}, \code{\link{across}}
#'
#' @family dplyr functions

aggregation_data <- function (data, agg, group_var, to_agg,
                              agg_name = NULL, percentage = FALSE,
                              percentage_name = NULL,
                              extra_col = FALSE, agg_extra = "sum") {

  if (is.null(data)) stop("The data object must be specified")
  if (is.null(group_var)) stop("The pooling variable(s) must be specified.")

  class_data <- class(data)
  ..percentage <- NULL
  if ("fringe" %in% class_data) data <- data$data

  if (extra_col) {

    extra_data <- data |>
      group_by(across(all_of(group_var))) |>
      summarise(across(where(~ is.numeric(.x) | is.integer(.x)), ~ aggregation(agg_extra, .x), .names = "{.col}"),
                across(where(~ is.factor(.x) | is.character(.x)), ~ paste_vector(.x), .names = "{.col}"))

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
                       ~ aggregation(agg, as.numeric(.x)),
                       .names = ifelse(is.null(agg_name), "{.col}", "{agg_name}")))

    if (percentage) {
      to_percentage <- to_agg
      if (!is.null(agg_name)) to_percentage <- agg_name
      if (is.null(percentage_name)) percentage_name <- paste0("..percentage", agg_name)
      result <- result |>
        mutate(across(all_of(to_percentage), ~ . / sum(.) * 100,
                      .names = "{percentage_name}"))
    }

  }

  if (extra_col) {
    result <- result |> left_join(extra_data)
  }

  result
}
