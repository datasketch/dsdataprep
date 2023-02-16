test_that("Aggregation", {
  data <- ggplot2::diamonds

  # Without percentage
  data_result <- aggregation_data(data = data,
                                 agg = "count",
                                 to_agg = NULL,
                                 name = "Conteo",
                                 group_var = c("cut", "color"))

  data_expect <- data |>
    dplyr::group_by(cut, color) |>
    dplyr::summarise(Conteo = dplyr::n())

  expect_equal(data_result, data_expect)

  # With percentage
  data_result <- aggregation_data(data = data,
                                  agg = "count",
                                  to_agg = NULL,
                                  name = "Conteo",
                                  group_var = c("cut", "color"),
                                  percentage = TRUE)


  data_expect <- data_expect |>
    dplyr::mutate(percentage = (Conteo / sum(Conteo))*100)

  expect_equal(data_result, data_expect)



  data_result <- aggregation_data(data = data,
                                  agg = "count",
                                  to_agg = NULL,
                                  name = "Conteo",
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
                                 name = c("Suma x", "Suma y"),
                                 group_var = c("cut"))

  data_expect <- data |>
    dplyr::group_by(cut) |>
    dplyr::summarise(`Suma x` = sum(x, na.rm = TRUE),
                     `Suma y` = sum(y, na.rm = TRUE))

  expect_equal(data_result, data_expect)



  data_result <- aggregation_data(data = data,
                                  agg = "sum",
                                  to_agg = c("x", "y"),
                                  name = c("Suma x", "Suma y"),
                                  group_var = c("cut"),
                                  percentage = TRUE)

  data_expect <- data |>
    dplyr::group_by(cut) |>
    dplyr::summarise(`Suma x` = sum(x, na.rm = TRUE),
                     `Suma y` = sum(y, na.rm = TRUE)) |>
    dplyr::mutate(`percentageSuma x` = `Suma x`/sum(`Suma x`, na.rm = T) * 100,
                  `percentageSuma y` = `Suma y`/sum(`Suma y`, na.rm = T) * 100)


  expect_equal(data_result, data_expect)




  data_result <- aggregation_data(data = data,
                                  agg = "sum",
                                  to_agg = c("x", "y"),
                                  name = NULL,
                                  group_var = c("cut"))

  data_expect <- data |>
    dplyr::group_by(cut) |>
    dplyr::summarise(x = sum(x, na.rm = TRUE),
                     y = sum(y, na.rm = TRUE))


  expect_equal(data_result, data_expect)




  data_result <- aggregation_data(data = data,
                                 agg = "mean",
                                 to_agg = "x",
                                 name = "Promedio",
                                 group_var = c("clarity", "cut"))

  data_expect <- data |>
    dplyr::group_by(clarity, cut) |>
    dplyr::summarise(Promedio = mean(x, na.rm = TRUE))

  expect_equal(data_result, data_expect)

})
