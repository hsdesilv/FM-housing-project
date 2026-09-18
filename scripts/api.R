library(vetiver)
library(pins)
library(plumber)

model_board <- board_folder(
  path = "models",
  versioned = TRUE
)

v_knn <- vetiver_pin_read(
  model_board,
  "fm_housing_knn"
)

pr() |>
  vetiver_api(v_knn) |>
  pr_run(port = 8000)

