
#' Add color to data columns in a data frame
#'
#' This function adds color to the data columns in a data frame or fringe,
#' using a color palette provided by the user.
#'
#' @param data A data frame to which colors will be added
#' @param palette_colors A named vector of color codes or a data frame with color codes and categories
#' @param palette_type A string specifying the name of the color palette to use
#' @param palette A vector of color codes to use as the color palette
#' @param color_by A string specifying the name of the column to use for coloring
#' @return A copy of the input data frame with colors added to the specified data column
#' @export
#' @examples
#' # Create some sample data
#' data <- data.frame(x = 1:5, y = c("A", "B", "C", "D", "E"))
#'
#' # Define the colors for the data columns
#' colors <- c("red", "green", "blue", "orange", "purple")
#'
#' # Apply the colors to the data
#' data_with_colors <- add_data_colors(data, colors, color_by = "y")
#'
#' # View the result
#' print(data_with_colors)
add_data_colors <- function(data, palette_colors = NULL, palette_type = "qualitative",
                            palette = NULL, color_by = NULL) {
  if (is.null(data)) {
    stop("The data object must be specified")
  }
  if (is.null(palette_colors) && is.null(palette_type) && is.null(palette)) {
    stop("No color information provided; either `palette_colors`, `palette_type`,
         or `palette` must be specified")
  }
  class_data <- class(data)

  if ("fringe" %in% class_data) data <- data$data

  if ("..colors" %in% names(data)) {
    return(data)
  } else {
    if (is.null(color_by)) {
      stop("You must specify the column you want to assign color")
    }
    column_color <- unique(data[[color_by]])
    length_column_color <- length(column_color)
    if (is.null(palette_colors)) {
      if (!is.null(palette)) {
        palette_colors <- suppressWarnings(
          paletero:::paleta(palette, n = length_column_color))
      }
    }
    class_palette_colors <- class(palette_colors)

      length_colors <- length(palette_colors)
      if (length_colors != length_column_color) {
        if (length_colors == 1) {
          palette_colors <- rep(palette_colors, length_column_color)
        } else if (length_colors > length_column_color) {
          palette_colors <- palette_colors[1:length_column_color]
        } else {
          palette_colors <- suppressWarnings(
            paletero:::paleta(palette_colors,
                              n = length_column_color,
                              type = palette_type)
          )
        }
      }

      data_colors  <- data.frame(..colors = palette_colors)
      data_colors[[color_by]] <- column_color
      data <- data |>
        left_join(data_colors, by = color_by)

  }
  data
}

