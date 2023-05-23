test_that("Data filter", {
  data <- iris
  dic <- homodatum::create_dic(data)
  names(dic) <- tolower(names(dic))
  names(data) <- dic$id
  data_result <- data_filter(data = data,
                             dic = dic,
                             var_inputs = list("species" = c("versicolor", "virginica" ),
                                               "sepal_length"= c(4, 5)))
  data_expected <- data |>
    filter(species %in% c("versicolor", "virginica"),
           sepal_length >= 4 & sepal_length <= 5)

  expect_equal(data_result, data_expected)

})
