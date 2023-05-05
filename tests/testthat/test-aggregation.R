test_that("Aggregation", {
  data <- ggplot2::diamonds

  dataptc <- data |> select(cut, color, price)
  data_result <- aggregation_data(data = dataptc,
                                  agg = "mean", to_agg = "price",
                                  group_var = c("cut", "color"),
                                  percentage = TRUE,
                                  percentage_col = "cut")


  data_result2 <- aggregation_data(data = dataptc,
                                  agg = "mean", to_agg = "price",
                                  group_var = c("cut", "color"),
                                  percentage = TRUE,
                                  percentage_col = "color")



  data <- data[1:100,]
  data$otra <- rep(1:5, 20)
  # Without percentage
  data_result <- aggregation_data(data = data,
                                  agg = "sum",
                                  to_agg = "otra",
                                  agg_name = "otra",
                                  group_var = c("cut"),
                                  extra_col = FALSE,
                                  extra_group = "clarity",
                                  collapse_columns = "clarity",
                                  numeric_collapse_columns = "y"
                                  #agg_extra = NULL
  )

  data_expect <- data |>
    dplyr::group_by(cut) |>
    dplyr::summarise(otra = sum(otra))

  expect_equal(data_result, data_expect)

  # With percentage
  data_result <- aggregation_data(data = data,
                                  agg = "count",
                                  to_agg = NULL,
                                  agg_name = "Conteo",
                                  group_var = c("cut", "color"),
                                  percentage = TRUE)


  data_expect <- data |>
    dplyr::group_by(cut, color) |>
    dplyr::summarise(Conteo = n()) |>
    dplyr::mutate(..percentage = (Conteo / sum(Conteo))*100)

  expect_equal(data_result, data_expect)



  data_result <- aggregation_data(data = data,
                                  agg = "count",
                                  to_agg = NULL,
                                  agg_name = "Conteo",
                                  group_var = c("cut", "color"),
                                  percentage = TRUE,
                                  percentage_name = "Porcentaje")


  data_expect <- data |>
    dplyr::group_by(cut, color) |>
    dplyr::summarise(Conteo = dplyr::n()) |>
    dplyr::mutate(Porcentaje = (Conteo / sum(Conteo))*100)

  expect_equal(data_result, data_expect)


  data_result <- aggregation_data(data = data,
                                  agg = "sum",
                                  to_agg = c("x", "y"),
                                  agg_name = c("Suma x", "Suma y"),
                                  group_var = c("cut"))

  data_expect <- data |>
    dplyr::group_by(cut) |>
    dplyr::summarise(`Suma x` = sum(x, na.rm = TRUE),
                     `Suma y` = sum(y, na.rm = TRUE))

  expect_equal(data_result, data_expect)



  data_result <- aggregation_data(data = data,
                                  agg = "sum",
                                  to_agg = c("x", "y"),
                                  agg_name = c("Suma x", "Suma y"),
                                  group_var = c("cut"),
                                  percentage = TRUE)

  data_expect <- data |>
    dplyr::group_by(cut) |>
    dplyr::summarise(`Suma x` = sum(x, na.rm = TRUE),
                     `Suma y` = sum(y, na.rm = TRUE)) |>
    dplyr::mutate(`..percentage Suma x` = `Suma x`/sum(`Suma x`, na.rm = T) * 100,
                  `..percentage Suma y` = `Suma y`/sum(`Suma y`, na.rm = T) * 100)


  expect_equal(data_result, data_expect)




  data_result <- aggregation_data(data = data,
                                  agg = "sum",
                                  to_agg = c("x", "y"),
                                  agg_name = NULL,
                                  group_var = c("cut"))

  data_expect <- data |>
    dplyr::group_by(cut) |>
    dplyr::summarise(x = sum(x, na.rm = TRUE),
                     y = sum(y, na.rm = TRUE))


  expect_equal(data_result, data_expect)




  data_result <- aggregation_data(data = data,
                                  agg = "mean",
                                  to_agg = "x",
                                  agg_name = "Promedio",
                                  group_var = c("clarity", "cut"))

  data_expect <- data |>
    dplyr::group_by(clarity, cut) |>
    dplyr::summarise(Promedio = mean(x, na.rm = TRUE))

  expect_equal(data_result, data_expect)

  data <- data.frame(name = c("D", "E", "F"), y = c(15, 25, 35))
  data_result <- aggregation_data(data = data,
                                  agg = "sum",
                                  to_agg = "y",
                                  group_var = "name")

  expect_equal(data_result, data, ignore_attr = TRUE)


  # dates <- seq(as.POSIXct("2022-01-01"), as.POSIXct("2022-01-10"), by = "day")
  # values <- rnorm(length(dates))
  # data <- data.frame(date = dates, value = values)
  # data_result <- aggregation_data(data = data,
  #                                 agg = "sum",
  #                                 to_agg = "value",
  #                                 group_var = "date")




})
