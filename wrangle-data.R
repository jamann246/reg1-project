data_raw <- read.csv('./data/raw_data.csv')

data <- 
  data_raw |> 
  dplyr::mutate(
    dplyr::across(
      dplyr::where(is.character), 
      ~factor(stringr::str_to_title(.x))
    ), 
    dplyr::across(
      .cols = c(CAEC, CALC), 
      .fns = ~factor(.x, level = c("No","Sometimes","Frequently","Always"))
    ),
    BMI = Weight/Height^2
    ) |> 
  dplyr::select(-c(Height, Weight, NObeyesdad))



save(data, file = "./data/clean_data.rda")
