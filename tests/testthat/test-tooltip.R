test_that("Tooltips work", {


  data <- head(cars)

  expect_equal(create_default_tpl(names(data)),
               "<b>speed:</b> {speed}<br/><b>dist:</b> {dist}")

  expect_equal(create_default_tpl(names(data), engine = "markdown"),
               "**speed:** {speed}\n\n**dist:** {dist}")

  new_labels <- c("Speed", "Dist")
  expect_equal(create_default_tpl(names(data), new_labels = new_labels),
               "<b>Speed:</b> {speed}<br/><b>Dist:</b> {dist}")

  expect_equal(
    create_default_tpl(names(data), new_labels = new_labels, engine = "markdown"),
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


  data <- ggplot2::diamonds

  v <- prep_tooltip(data, format_num = "1,234.9")

  expect_equal(v[1],
               "<b>carat:</b> 0.2<br/><b>cut:</b> Ideal<br/><b>color:</b> E<br/><b>clarity:</b> SI2<br/><b>depth:</b> 61.5<br/><b>table:</b> 55<br/><b>price:</b> 326<br/><b>x:</b> 4<br/><b>y:</b> 4<br/><b>z:</b> 2.4"
  )

  v <- prep_tooltip(data, format_num = "1,234.9", opts_format_num = list(si_prefix = TRUE))

  expect_equal(v[100],
               "<b>carat:</b> 0.8<br/><b>cut:</b> Premium<br/><b>color:</b> H<br/><b>clarity:</b> SI1<br/><b>depth:</b> 61.5<br/><b>table:</b> 58.0<br/><b>price:</b> 2.8K<br/><b>x:</b> 6.0<br/><b>y:</b> 5.9<br/><b>z:</b> 3.7"
  )

  v <- prep_tooltip(data,
                    format_num = "1345.78",
                    opts_format_num = list(si_prefix = TRUE),
                    format_cat = "UPPER",
                    tooltip = "Precio: {price} <br/> Tipo: {cut}")

  expect_equal(v[100],
               "Precio: 2.76K <br/> Tipo: PREMIUM"
  )


  data <- iris
  names(data) <- gsub("\\.", " ", names(data))
  v <- prep_tooltip(data, as_df = TRUE)


})
