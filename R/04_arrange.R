

#' Sort and wrap data by a categorical variable and numeric variable
#'
#' @param data A data frame
#' @param col_cat A character vector specifying the name of the categorical variable to sort and wrap by.
#' @param col_num A character vector specifying the name of the numeric variable to sort by.
#' @param order A vector specifying the order of the categories.
#' @param order_legend A vector specifying the order of the categories in legend.
#' @param label_wrap An integer specifying the width of the wrapped label.
#' @param label_wrap_legend An integer specifying the width of the wrapped legend items.
#' @param new_line A string that will be used to replace newline characters.
#' @param sort A character vector that specifies the sorting order.
#' @param slice_n an integer indicating the number of top rows to keep for each group
#' @param intra_cat a boolean indicating whether to slice within each category or across all categories
#' @param index_names the name(s) of the new(s) index column(s)
#'
#' @return A data frame sorted and/or wrapped by col_cat and/or col_num.
#'
#' @importFrom purrr map
#'
#' @export
wrap_sort_data <- function(data, col_cat = NULL, col_num = NULL, order = NULL,
                           order_legend = NULL, label_wrap = NULL, label_wrap_legend = NULL,
                           new_line = "<br/>", sort = NULL, sort_by_cat = FALSE,
                           slice_n = NULL, intra_cat = TRUE, index_names = NULL) {

  if (is.null(data)) {
    stop("The data object must be specified")
  }

  class_data <- class(data)

  if ("fringe" %in% class_data) data <- data$data


  if (!is.null(col_num)) {
    if (!is.null(sort)) {
      print("in sort")
      data <- data |>
        numeric_sort(col_num, col_cat, sort = sort,
                     slice_n = slice_n, intra_cat = intra_cat
        )
    }
  }


  if (!is.null(col_cat)) {

    col_cat_class <- map(data[,col_cat], ~class(.x)) |> unlist()

    if (!any(col_cat_class %in% c("character", "factor", "hd_Cat"))) {
      stop("col_cat must be character or factor")
    }

    if (!is.null(order)) {
      unique_vals <- unique(data[[col_cat[length(col_cat)]]])
      order <- union(order, unique_vals[!is.na(unique_vals)])
      order <- c(order, unique_vals[!is.na(unique_vals)])
      data <- data[order(match(data[[col_cat[length(col_cat)]]], order)), ]
    }

    if (!is.null(order_legend)) {
      unique_vals <- unique(data[[col_cat[1]]])
      order_legend <- union(order_legend, unique_vals[!is.na(unique_vals)])
      order_legend <- c(order_legend, unique_vals[!is.na(unique_vals)])
      data <- data[order(match(data[[col_cat[1]]], order_legend)), ]
    }


    if (!is.null(index_names)) {
      data <- add_group_index(data, col_cat, index_names)
    }


    if (!is.null(label_wrap)) {
      data[[col_cat[length(col_cat)]]] <- data[[col_cat[length(col_cat)]]] |>
        stringr::str_wrap(width = label_wrap) |>
        stringr::str_replace_all(pattern = "\\\n",
                                 replacement = new_line)

    }


    if (!is.null(label_wrap_legend)) {
      data[[col_cat[1]]] <- data[[col_cat[1]]] |>
        stringr::str_wrap(width = label_wrap_legend) |>
        stringr::str_replace_all(pattern = "\\\n",
                                 replacement = new_line)

    }
  }
  data
}
