#' Create list of names from input data or dictionary
#'
#' This function creates a list of names based on the input data or dictionary.
#' If a dictionary is provided, it is used to map variable IDs to their
#' corresponding names. Otherwise, the data names are used as the names of the list.
#'
#' @param data_names A character vector of data names
#' @param dic A data frame containing variable IDs and their corresponding names
#'
#' @return A list of names
#' @importFrom rlang set_names
#' @examples
#' data_names <- c("x", "y", "z")
#' dic <- data.frame(id = c("x", "y", "z"), label = c("Variable X", "Variable Y", "Variable Z"))
#' create_nms(data_names, dic)
#'
#' @export
create_nms <- function(data_names = NULL, dic = NULL) {
  if (!is.null(dic)) {
    ls <- set_names(dic$label, dic$id)
  } else {
    ls <- set_names(data_names, data_names)
  }
  ls |> as.list()
}


#' Create tooltip string from list of labels
#'
#' This function takes a list of labels and returns a tooltip string that can be
#' used to display the labels in a plot or other visualization.
#' Each label in the list is mapped to its name, and the
#' resulting strings are concatenated with line breaks ("<br/>") between them.
#'
#' @param nms A list of labels
#' @param tooltip A tooltip template
#'
#' @return A tooltip string
#'
#' @examples
#' nms <- list(x = "Variable X", y = "Variable Y", z = "Variable Z")
#' labels_map(nms)
#'
#' @export
labels_map <- function(nms, tooltip = NULL) {
  if (is.null(tooltip)) {
  if (!is.list(nms)) stop("Input must be a list")
  if (!all(sapply(nms, is.character))) {
    stop("All elements of input list must be character strings")
  }
  tooltip <- purrr::imap(nms, ~ paste0("<b>",.x, ":</b> {", .y, "}")) %>%
    unlist() %>%
    paste(collapse = "<br/>")
  }
  tooltip
}




#' Add tooltips to data columns in a data frame
#'
#' This function adds tooltips to the data columns in a data frame, using labels
#' provided by the user.
#'
#' @param data A data frame to which tooltips will be added
#' @param nms A named character vector containing the labels for the data columns
#' @param tooltip A tooltip template
#' @return A copy of the input data frame with tooltips added to each data column
#' @export
#' @import dplyr
#' @import glue
#' @import htmltools
#' @importFrom purrr imap
#' @importFrom purrr map
#' @importFrom htmltools HTML
#' @importFrom dplyr mutate
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#' @examples
#' # Create some sample data
#' data <- data.frame(x = 1:5, y = c("A", "B", "C", "D", "E"))
#'
#' # Define the labels for the data columns
#' nms <- create_nms(data_names = names(data))
#'
#' # Apply the tooltip to the data
#' data_with_tooltip <- add_data_tooltip(data, nms)
#'
#' # View the result
#' print(data_with_tooltip)
add_data_tooltip <- function(data, nms = NULL, tooltip = NULL) {
  if (is.null(data)) stop("The data object must be specified")
  class_data <- class(data)
  if ("fringe" %in% class_data) data <- data$data
  col_names <- grep("^[^nms]|^[^tooltip]", names(data), value = TRUE)
  if (length(col_names) == 0) {
    warning("No data columns found; returning original data")
    return(data)
  }
  data %>%
    mutate(..labels = glue::glue(
     labels_map(nms = nms, tooltip = tooltip)) %>%
        lapply(htmltools::HTML)
    )
}




