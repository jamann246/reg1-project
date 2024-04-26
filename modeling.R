load("./data/clean_data.rda")

library(ggplot2)
theme_set(theme_minimal())

# doing some additional pre-processing 
full_rec <- 
  recipes::recipe(BMI ~ ., data = data) |> 
  recipes::step_normalize(recipes::all_numeric_predictors()) |> 
  recipes::step_other(MTRANS, CALC, threshold = 0.01) |> 
  recipes::step_dummy(recipes::all_nominal_predictors()) 

prepped_data <- recipes::prep(full_rec) |> recipes::bake(data)

table(data$MTRANS)
colnames(prepped_data)

# starting with a full model - this should be the first of the two models we'll need
full_mod <- lm(BMI ~ ., data = prepped_data)
summary(full_mod)

full_mod_ <- lm(BMI~., data = data)
summary(full_mod_)
# perform best subset selection
best_subset <- leaps::regsubsets(BMI ~ ., data = prepped_data, nvmax = 20, method = "exhaustive")
results <- summary(best_subset)

# extract and plot results
n <- nrow(prepped_data)
p <- 20
results_df <- 
  tibble::tibble(
    predictors = 1:20,
    adj_R2 = results$adjr2,
    bic = results$bic, 
    aic = n*log(results$rss/n) + (1:20)*2
  ) 

results_df |> 
  ggplot(aes(predictors, aic, color = aic)) +
  geom_point(show.legend = F) + 
  labs(
    title = "AIC vs. Number of Predictors", 
    x = "Number of Predictors",
    y = "AIC"
  )

# training the bic selected model
form <- paste("BMI~", paste(names(which(results$which[which.min(results_df$aic),-1])), collapse = "+")) |> 
  as.formula()

aic_mod <- lm(form, data = prepped_data)
summary(aic_mod)


results_df |> 
  ggplot(aes(predictors, adj_R2, color = adj_R2)) +
  geom_point(show.legend = F) + 
  labs(
    title = "Adjusted R2 vs. Number of Predictors", 
    x = "Number of Predictors",
    y = "Adjusted R2"
  )
# training the bic selected model
form <- paste("BMI~", paste(names(which(results$which[which.max(results_df$adj_R2),-1])), collapse = "+")) |> 
  as.formula()

r2_mod <- lm(form, data = prepped_data)
summary(r2_mod)
