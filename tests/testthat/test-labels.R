test_that("Labels", {
  data_names <- c("x", "y", "z")
  dic <- data.frame(id = c("x", "y", "z"),
                    label = c("Variable X", "Variable Y", "Variable Z"))


  data_result <- create_nms(data_names = data_names, dic = NULL)
  data_expect <- setNames(data_names, data_names) |> as.list()
  expect_equal(data_result, data_expect)

  data_result <- create_nms(data_names = NULL, dic)
  data_expect <- setNames(dic$label, dic$id) |> as.list()
  expect_equal(data_result, data_expect)




  data_result <- labels_map(nms = data_result, tooltip = NULL)
  expect_equal(data_result,
               "<b>Variable X:</b> {x}<br/><b>Variable Y:</b> {y}<br/><b>Variable Z:</b> {z}")


  data_result <- labels_map(nms = NULL, tooltip = "Variable: {x}")
  expect_equal(data_result,
               "Variable: {x}")


  data <-  ggplot2::diamonds
  nms <- create_nms(data_names = names(data)[1])
  data_result <- add_data_tooltip(data = data, nms = nms)
  data_expect <- paste0("<b>carat:</b> ", data$carat)
  expect_equal(data_result$labels |> unlist(), data_expect)

})
