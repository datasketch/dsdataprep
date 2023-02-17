#' Sorts a data frame by a numeric column.
#'
#' This function sorts a data frame by a numeric column. It can also group by
#' one or more categorical variables before sorting.
#'
#' @param data a data frame to sort.
#' @param col_num the name of the numeric column to sort by.
#' @param col_cat the name of the categorical column(s) to group by before sorting.
#' @param sort the order to sort the data frame in (either "asc" or "desc").
#'
#' @return the sorted data frame.
#'
#' @examples
#' numeric_sort(mtcars, "mpg", "cyl", "desc")
#'
#' @import dplyr
#' @importFrom tidyr all_of
#' @keywords internal
numeric_sort <- function(data, col_num, col_cat = NULL, sort = NULL) {

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
        dplyr::arrange(desc(across(all_of(col_num))), .by_group = is.null(col_cat) == FALSE)
    } else {
      data <- data |>
        dplyr::arrange(across(all_of(col_num)), .by_group = is.null(col_cat) == FALSE)
    }

  }

  data
}



#' Wraps text and sorts data by category
#'
#' @param data a data.frame
#' @param col_cat a character string specifying the name of the col_catumn containing categories
#' @param order a character vector specifying the order in which categories should be sorted
#' @param label_wrap an integer specifying the maximum number of characters per line, or NULL to disable wrapping
#' @param new_line a character string specifying the HTML line break character to use
#'
#' @return a data.frame with wrapped text and sorted categories
wrap_sort_data <- function(data, col_cat = NULL, col_num = NULL, order = NULL,
                           label_wrap = NULL, new_line = "<br/>", sort = NULL) {

  if (is.null(data)) {
    stop("The data object must be specified")
  }

  if (!is.null(col_num)) {
    if (!is.null(sort)) {
      data <- data |> numeric_sort(sort = sort)
    }
  }


  if (!is.null(col_cat)) {
    col_cat_class <- class(data[[col_cat]])
    if (!any(col_cat_class %in% c("character", "factor", "hd_Cat"))) {
      stop("col_cat must be character or factor")
    }

    if (!is.null(order)) {
      unique_vals <- unique(data[[col_cat]])
      order <- union(order, unique_vals[!is.na(unique_vals)])
      order <- c(order, unique_vals[!is.na(unique_vals)])
      data <- data[order(match(data[[col_cat]], order)), ]
    }

    if (!is.null(label_wrap)) {
      data[[col_cat]] <- data[[col_cat]] |> stringr::str_wrap(width = label_wrap) |>
        stringr::str_replace_all(pattern = "\\\n",
                                 replacement = new_line)

    }
  }
  data
}
