library(ggplot2)
theme_set(theme_minimal())

load("./data/clean_data.rda")

psych::describe(data) |> 
  dplyr::select(-c(median, trimmed, mad, skew, kurtosis,se))

# univariate
# numeric data
data |> 
  tidyr::pivot_longer(cols = dplyr::where(is.numeric)) |> 
  ggplot2::ggplot(ggplot2::aes(value, fill = name)) + 
  ggplot2::geom_histogram() + 
  ggplot2::facet_wrap(~name, scales = "free") + 
  ggplot2::theme(legend.position = "none", 
                 axis.title = element_blank())

# CH2O, FAF, FCVC, NCP, TUE are discrete
# Age, BMI, Height, Weight are continuous, normal or log normal distributed


# factor data
data |> 
  tidyr::pivot_longer(cols = dplyr::where(is.factor)) |> 
  ggplot(aes(value, fill = name)) + 
  geom_bar() + 
  coord_flip() + 
  facet_wrap(~name, scales = "free") + 
  theme(legend.position = "none")

data |> 
  dplyr::mutate(Transportation_Type = dplyr::if_else(
    Transportation_Type %in% c("Bike","Motorbike"), 
    "Other",
    Transportation_Type
  ) |> as.factor() |> forcats::fct_infreq()
  ) |> 
  tidyr::pivot_longer(cols = dplyr::where(is.factor)) |> 
  ggplot(aes(value, fill = name)) + 
  geom_bar() + 
  coord_flip() + 
  facet_wrap(~name, scales = "free") + 
  theme(legend.position = "none")

# some *very* infrequent methods of transit
# smoking is not at all common

# bivariate

corrplot::corrplot(
  corr = cor(prepped_data |> dplyr::select(dplyr::where(is.numeric))), 
  method = "pie", 
  type = "upper"
  )

data |> 
  ggplot(aes(Age, BMI)) + 
  geom_point() + 
  geom_smooth() # looks decent, not quite linear

data |> 
  ggplot(aes(FCVC, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") # moderate slope, looks linear

data |> 
  ggplot(aes(NCP, BMI)) + 
  geom_point() + 
  geom_smooth() # not really anything, could be polynomial

data |> 
  ggplot(aes(CH2O, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") # really not much here

data |> 
  ggplot(aes(FAF, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") # could be something

data |> 
  ggplot(aes(TUE, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") # again not much here

# looking for interactions

data |> 
  ggplot(aes(Age, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~family_history_with_overweight) # no

data |> 
  ggplot(aes(Age, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~CAEC)

data |> 
  ggplot(aes(Age, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~CALC) # no, potentially consolidate CALC

data |> 
  ggplot(aes(Age, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~MTRANS)

data |> 
  ggplot(aes(FCVC, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") # moderate slope, looks linear

data |> 
  ggplot(aes(NCP, BMI)) + 
  geom_point() + 
  geom_smooth() # not really anything, could be polynomial

data |> 
  ggplot(aes(CH2O, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") # really not much here

data |> 
  ggplot(aes(FAF, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") # could be something

data |> 
  ggplot(aes(TUE, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") # again not much here

data |> 
  ggplot(aes(SCC, BMI)) + 
  geom_boxplot() + 
  geom_smooth() + 
  facet_wrap(~Gender) 



data |> 
  # dplyr::filter(Transportation_Type %in% c("Bike","Motorbike")) |> 
  ggplot(aes(Transportation_Type, BMI)) + 
  geom_boxplot()
