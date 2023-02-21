test_that("Tooltips work", {


  data <- head(cars)

  expect_equal(create_default_tpl(data),
               "<b>speed:</b> {speed}<br/><b>dist:</b> {dist}")

  expect_equal(create_default_tpl(data, engine = "markdown"),
               "**speed:** {speed}\n\n**dist:** {dist}")

  new_labels <- c("Speed", "Dist")
  expect_equal(create_default_tpl(data, new_labels = new_labels),
               "<b>Speed:</b> {speed}<br/><b>Dist:</b> {dist}")

  expect_equal(
    create_default_tpl(data, new_labels = new_labels, engine = "markdown"),
    "**Speed:** {speed}\n\n**Dist:** {dist}"
  )

  v <- prep_tooltip(data)
  v
  expect_equal(
    v[c(1,3)],
    c("<b>speed:</b> 4<br/><b>dist:</b> 2",
      "<b>speed:</b> 7<br/><b>dist:</b> 4"
    )
  )

  data <- head(cars)
  data[2,] <- c(NA, NA)

  v <- prep_tooltip(data)
  expect_equal(v[2], "(NA)")
  v <- prep_tooltip(data, na_label = "NAN")
  expect_equal(v[2], "NAN")

  data$label <- paste("Label", 1:nrow(data))
  v <- prep_tooltip(data, na_row_default_column = "label")
  v
  expect_equal(v[1:2],
               c("<b>speed:</b> 4<br/><b>dist:</b> 2","Label 2")
               )


})
