test_that("Add colors", {
  data <- ggplot2::diamonds
  # colors <- c('#3a3766', '#5b5fde', '#ec58c9', '#ffa92a', '#fcd381', '#78e1b4')
  # data_result <- add_data_colors(data, palette_colors = colors, color_by = "clarity")
  # data_result <- add_data_colors(data, palette = "Set1", color_by = "clarity")
  # data_result <- add_data_colors(data_result)


  colors <- '#3a3766'
  data_result <- add_data_colors(data, palette_colors = colors, color_by = "clarity")
  expect_equal(unique(data_result$..colors), colors)

  data <- data |> select(carat, x)
  data_result <- add_data_colors(data, palette_colors = "#FAFAFA")
  expect_equal(unique(data_result$..colors), "#FAFAFA")
})

