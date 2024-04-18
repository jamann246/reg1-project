load("./data/clean_data.rda")

library(ggplot2)
theme_set(theme_bw())

# doing some additional pre-processing 
full_rec <- 
  recipes::recipe(BMI ~ ., data = data) |> 
  recipes::step_normalize(recipes::all_numeric_predictors()) |> 
  recipes::step_other(MTRANS, CALC, threshold = 0.01) |> 
  recipes::step_dummy(recipes::all_nominal_predictors()) 

prepped_data <- recipes::prep(full_rec) |> recipes::bake(data)


# starting with a full model - this should be the first of the two models we'll need
full_mod <- lm(BMI ~ ., data = prepped_data)
summary(full_mod)

# perform best subset selection
best_subset <- leaps::regsubsets(BMI ~ ., data = prepped_data, nvmax = 20)
results <- summary(best_subset)

# extract and plot results
results_df <- 
  tibble::tibble(
    predictors = 1:20,
    adj_R2 = results$adjr2,
    bic = results$bic
  ) 

results_df |> 
  ggplot(aes(predictors, bic, color = bic)) +
  geom_point(show.legend = F) + 
  labs(
    title = "BIC vs. Number of Predictors", 
    x = "Number of Predictors",
    y = "BIC"
  )

# training the bic selected model
form <- paste("BMI~", paste(names(which(results$which[which.min(results_df$bic),-1])), collapse = "+")) |> 
  as.formula()

bic_mod <- lm(form, data = prepped_data)
summary(bic_mod)
