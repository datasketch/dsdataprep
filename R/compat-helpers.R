# es una funcion que es ejecutada por do.call puede ser sum, mean, max, min, sd, etc
#' @keywords internal
aggregation <- function(aggregation, na_rm = TRUE, ...) {
  if (is.null(aggregation) || is.na(aggregation)) {
    stop("The aggregation type must be specified.")
  }

  result <- do.call(aggregation, list(..., na.rm = na_rm))

  return(result)
}


#' @keywords internal
paste_vector <- function(x, collapse = ",") {
  paste0(trimws(as.character(unique(x))), collapse = collapse)
}


#' Sorts a data frame by a numeric column.
#'
#' This function sorts a data frame by a numeric column. It can also group by
#' one or more categorical variables before sorting.
#'
#' @param data a data frame to sort.
#' @param col_num the name of the numeric column to sort by.
#' @param col_cat the name of the categorical column(s) to group by before sorting.
#' @param sort the order to sort the data frame in (either "asc" or "desc").
#' @param slice_n an integer indicating the number of top rows to keep for each group
#' @param intra_cat a boolean indicating whether to slice within each category or across all categories
#'
#' @return the sorted data frame.
#'
#' @import dplyr
#' @importFrom tidyr all_of
#' @keywords internal
numeric_sort <- function(data, col_num, col_cat = NULL,
                         sort = NULL,
                         slice_n = NULL, intra_cat = TRUE) {

  if (is.null(data)) {
    stop("The data object must be specified")
  }

  if (length(col_num) == 1) {
    col_num_class <- class(data[[col_num]])
    if (!(col_num_class %in% c("numeric", "integer"))) {
      stop("col_num must be numeric")
    }
    if (sort == "no") sort <- NULL

    if (!is.null(sort)) {
      sort_by_cat <- FALSE
        if (!is.null(col_cat)) {
          sort_by_cat <- TRUE
          # data <- data |>
          #   group_by(across(all_of(col_cat)))
        }

      if (sort == "desc") {
        data <- data |>
          arrange(desc(across(all_of(col_num))), .by_group = sort_by_cat)
      } else {
        data <- data |>
          arrange(across(all_of(col_num)), .by_group = sort_by_cat)
      }
    }
  }

  if (!is.null(slice_n)) {
    if (!intra_cat) {
      data <- data |> ungroup()
    }
    data <- data |> slice_head(n = slice_n)
  }

  data
}


#' Add group index to a data frame based on specified group columns
#' @keywords internal
add_group_index <- function(data, group_cols, index_cols = NULL) {

  if (is.null(index_cols)) {
    index_cols <- paste0("index_", group_cols)
  }

  for (i in seq_along(group_cols)) {
    # Convert the group column to factor with the levels in the order they appear in the data
    levels <- unique(data[[group_cols[i]]])
    data[[group_cols[i]]] <- factor(data[[group_cols[i]]], levels = levels)

    # Add the index column by group
    data <- data %>%
      group_by(!!sym(group_cols[i])) %>%
      mutate(!!sym(index_cols[i]) := cur_group_id() - 1) %>%
      ungroup()
  }

  # Return the modified data frame
  data
}
