# Empty garbage collector
base::gc()



# __________________________________________________________________________________________________
# 1. Load required packages
List__packages <- base::list("readr")
base::lapply(X = List__packages, FUN = base::library, character.only = T, warn.conflicts = T)



# __________________________________________________________________________________________________
# 2. Load train.csv and test.csv
Df__train <- readr::read_csv(
  file = "data/train/train.csv", col_types = readr::cols(.default = readr::col_character())) |>
  readr::stop_for_problems() |> base::as.data.frame()
Df__test <- readr::read_csv(
  file = "data/test/test.csv", col_types = readr::cols(.default = readr::col_character())) |>
  readr::stop_for_problems() |> base::as.data.frame()



# __________________________________________________________________________________________________
# 3. Feature engineering







