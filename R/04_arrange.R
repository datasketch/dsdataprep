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
numeric_sort <- function(data, col_num, col_cat = NULL, sort = NULL,
                         slice_n = NULL, intra_cat = TRUE) {

  if (is.null(data)) {
    stop("The data object must be specified")
  }

  col_num_class <- class(data[[col_num]])
  if (!(col_num_class %in% c("numeric"))) {
    stop("col_num must be numeric")
  }

  if (!is.null(sort)) {
    if (!is.null(col_cat)) {
      data <- data |>
        group_by(across(all_of(col_cat)))
    }
    if (sort == "desc") {
      data <- data |>
        arrange(desc(across(all_of(col_num))), .by_group = is.null(col_cat) == FALSE)
    } else {
      data <- data |>
        arrange(across(all_of(col_num)), .by_group = is.null(col_cat) == FALSE)
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



#' Sort and wrap data by a categorical variable and numeric variable
#'
#' @param data A data frame
#' @param col_cat A character vector specifying the name of the categorical variable to sort and wrap by.
#' @param col_num A character vector specifying the name of the numeric variable to sort by.
#' @param order A vector specifying the order of the categories.
#' @param label_wrap An integer specifying the width of the wrapped label.
#' @param new_line A string that will be used to replace newline characters.
#' @param sort A character vector that specifies the sorting order.
#' @param slice_n an integer indicating the number of top rows to keep for each group
#' @param intra_cat a boolean indicating whether to slice within each category or across all categories
#' @param index_names the name(s) of the new(s) index column(s)
#'
#' @return A data frame sorted and/or wrapped by col_cat and/or col_num.
#'
#' @export
wrap_sort_data <- function(data, col_cat = NULL, col_num = NULL, order = NULL,
                           label_wrap = NULL, new_line = "<br/>", sort = NULL,
                           slice_n = NULL, intra_cat = TRUE, index_names = NULL) {

  if (is.null(data)) {
    stop("The data object must be specified")
  }

  class_data <- class(data)

  if ("fringe" %in% class_data) data <- data$data


  if (!is.null(col_num)) {
    if (!is.null(sort)) {
      data <- data |> numeric_sort(col_num, col_cat, sort = sort,
                                   slice_n = slice_n, intra_cat = intra_cat
                                   )
    }
  }


  if (!is.null(col_cat)) {
    # col_cat_class <- class(data[[col_cat]])
    # if (!any(col_cat_class %in% c("character", "factor", "hd_Cat"))) {
    #   stop("col_cat must be character or factor")
    # }
    #
    # if (!is.null(order)) {
    #   unique_vals <- unique(data[[col_cat]])
    #   order <- union(order, unique_vals[!is.na(unique_vals)])
    #   order <- c(order, unique_vals[!is.na(unique_vals)])
    #   data <- data[order(match(data[[col_cat]], order)), ]
    # }


    if (!is.null(index_names)) {
    data <- add_group_index(data, col_cat, index_names)
    }


    # if (!is.null(label_wrap)) {
    #   data[[col_cat]] <- data[[col_cat]] |> stringr::str_wrap(width = label_wrap) |>
    #     stringr::str_replace_all(pattern = "\\\n",
    #                              replacement = new_line)
    #
    # }
  }
  data
}
