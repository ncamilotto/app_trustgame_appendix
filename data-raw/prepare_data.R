# data-raw/prepare_data.R


load("C:/Users/ncamilotto/Documents/R_projects/app.trustgame.appendix/data-raw/dataframes.RData")

usethis::use_data(
  alluv_content_shiny,
  ref_for_alluv,
  references_collapse,
  university_location_en,
  overwrite = TRUE
)
