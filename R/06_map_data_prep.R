data_map_draw <- function(data = NULL,
                          var = NULL,
                          opts = NULL) {

  map_name <- opts$map_name
  if(is.null(map_name)) stop("No map name provided, see geodato::available_maps()")
  dgeo <- geodato::gd_tj(map_name)


  if(!is.null(data)){
    d <- data
    if ("hdtable" %in% class(data)) d <- d$data

    d <- tryCatch({
      geodato::gd_match(d = d, map_name = map_name)
    },
    error = function(e) {
      tryCatch({
        geodato::gd_match_codes(d = d, map_name = map_name)
      },
      error = function(e) {
        stop("no geographic variables are identified")
      })
    })

    if (!"..labels" %in% names(d)) {
      d$..labels <- prep_tooltip(data = d,
                                 tooltip = opts$tooltip_template,
                                 new_labels = NULL,
                                 engine = "html",
                                 as_df = FALSE,
                                 na_row_default_column = NULL,
                                 na_row_default_value = NULL,
                                 na_label = opts$na_label,
                                 format_num = opts$format_sample_num,
                                 format_cat = opts$format_sample_cat,
                                 format_date = opts$format_sample_dat)
    }


    if (!is.null(var)) {
      col <- geodato::parse_col(d, var)
      if (class(d[[col]]) %in% c("character", "factor")) {
        d$..var <- as.integer(factor(d[[col]]))
      } else {
        d$..var <- d[[col]]
      }
    }
    dgeo <- dgeo |>
      dplyr::left_join(d, by = c(id = "..gd_id", name = "..gd_name"))

  }

  dgeo


}
