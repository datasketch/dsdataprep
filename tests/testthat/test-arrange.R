test_that("Sort numeric and categorical var", {
  data <- ggplot2::diamonds
  data_result <- numeric_sort(data, col_num = "carat", sort = "desc")
  data_expect <- data |> dplyr::arrange(desc(carat))
  expect_equal(data_result, data_expect)

  data_result <- numeric_sort(data, col_num = "carat", sort = "asc")
  data_expect <- data |> dplyr::arrange(carat)
  expect_equal(data_result, data_expect)

  data_result <- numeric_sort(data, col_num = "carat", col_cat = "cut", sort = "asc")
  data_expect <- data %>%
    group_by(cut) %>%
    arrange(carat, .by_group = TRUE)
  expect_equal(data_result, data_expect)

  data_result <- numeric_sort(data, col_num = c("carat"), col_cat = c("cut", "clarity"), sort = "desc")
  data_expect <- data %>%
    group_by(cut, clarity) %>%
    arrange(-carat, .by_group = TRUE)
  expect_equal(data_result, data_expect)



  data_result <- wrap_sort_data(data, col_cat = "cut", order = NULL)
  expect_equal(data_result, data)

  data_result <- wrap_sort_data(data, col_cat = "cut", order = c("Good", "Ideal"))
  data_expect <- c("Good", "Ideal")
  expect_equal(as.character(unique(data_result$cut)[1:2]), data_expect)


  #wrap_sort_data(data, col_cat = "cut", order = NULL, label_wrap = 2)
})
