#' @import dplyr
#' @importFrom rlang :=



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
#' @param name A character vector of new names for the summary variables
#' @param percentage A logical value indicating whether to calculate percentages
#' @param percentage_name A character vector of new names for percentage columns
#'
#' @return A data frame containing the aggregated data and summary statistics
#' @export
#'
#' @examples
#' data(mtcars)
#' aggregation_data(mtcars, "mean", c("cyl", "gear"), c("mpg", "hp"))
#' @export
aggregation_data <- function (data, agg, group_var, to_agg,
                              name = NULL, percentage = FALSE,
                              percentage_name = NULL) {

  if (is.null(data)) stop("The data object must be specified")
  if (is.null(group_var)) stop("The pooling variable(s) must be specified.")

  class_data <- class(data)

  if ("fringe" %in% class_data) data <- data$data

  if (agg == "count") {
    result <- data |>
      group_by(across(all_of(group_var))) |>
      summarise(count = n())
    if (percentage) {
      result <- result |>
        mutate(percentage = (count / sum(count))*100)
      if (!is.null(percentage_name)) {
        result <- result |> rename(!!percentage_name := percentage)
      }
    }
    if (!is.null(name)) {
      result <- result |> rename(!!name := count)
    }
  } else {
    result <- data |>
      group_by(across(all_of(group_var))) |>
      summarise(across(all_of(to_agg),
                       ~ aggregation(agg, .x),
                       .names = ifelse(is.null(name), "{.col}", "{name}")))

    if (percentage) {
      to_percentage <- to_agg
      if (!is.null(name)) to_percentage <- name
      if (is.null(percentage_name)) percentage_name <- paste0("percentage", name)
      result <- result |>
        mutate(across(all_of(to_percentage), ~ . / sum(.) * 100,
                      .names = "{percentage_name}"))
    }

  }

  result
}
