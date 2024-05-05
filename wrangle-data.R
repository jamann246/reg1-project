data_raw <- read.csv('./data/raw_data.csv')

dic <- openxlsx::read.xlsx("./data/data_dictionary.xlsx")

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
    dplyr::across(
      .cols = c(FCVC, TUE, NCP, CH2O, FAF, Age), 
      .fns = as.integer
    ),
    MTRANS = forcats::fct_inorder(factor(MTRANS)),
    BMI = Weight/(Height^2)
    ) |> 
  dplyr::select(-c(Height, Weight, NObeyesdad))

names(data) <- dic$Name

save(data, file = "./data/clean_data.rda")


data_dirty <- 
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
    MTRANS = forcats::fct_inorder(factor(MTRANS)),
    BMI = Weight/(Height^2)
  ) |> 
  dplyr::select(-c(Height, Weight, NObeyesdad))

names(data_dirty) <- dic$Name

save(data_dirty, file = "./data/dirty_data.rda")

