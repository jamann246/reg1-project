load("./data/final_model.rds")

x <- model.matrix(bic_mod2) |> 
  as.data.frame() |> 
  dplyr::summarise(dplyr::across(dplyr::everything(), median)) |> 
  dplyr::select(-1)

predict(bic_mod2, x) |> exp()
