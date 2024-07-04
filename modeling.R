load("./data/clean_data.rda")

library(ggplot2)
theme_set(theme_minimal())

# doing some additional pre-processing 
full_rec <- 
  recipes::recipe(BMI ~ ., data = data) |> 
  # recipes::step_normalize(recipes::all_numeric_predictors()) |> 
  recipes::step_other(Transportation_Type, Alcohol_Consumption, threshold = 0.01) |> 
  recipes::step_dummy(recipes::all_nominal_predictors())

prepped_data <- recipes::prep(full_rec) |> recipes::bake(data)

save(prepped_data, file = "./data/prepped_data.rda")

# starting with a full model - this should be the first of the two models we'll need
full_mod <- lm(BMI ~ ., data = prepped_data)
summary(full_mod)
BIC(full_mod)

# perform best subset selection
best_subset <- leaps::regsubsets(BMI ~ ., data = prepped_data, nvmax = 20, method = "exhaustive")
results <- summary(best_subset)

# extract and plot results
n <- nrow(prepped_data)
p <- 20
results_df <- 
  tibble::tibble(
    predictors = 1:p,
    adj_R2 = results$adjr2,
    bic = results$bic, 
    aic = n*log(results$rss/n) + (1:p)*2
  ) 

results_df |> 
  ggplot(aes(predictors, aic, color = aic)) +
  geom_point(show.legend = F) + 
  labs(
    title = "AIC vs. Number of Predictors", 
    x = "Number of Predictors",
    y = "AIC"
  )

# training the aic selected model
form <- paste("BMI~", paste(names(which(results$which[which.min(results_df$aic),-1])), collapse = "+")) |> 
  as.formula()

aic_mod <- lm(form, data = prepped_data)
summary(aic_mod)


results_df |> 
  ggplot(aes(predictors, bic, color = bic)) +
  geom_point(show.legend = F) + 
  geom_point(data = results_df[which.min(results_df$bic), ], color="red", 
             size=3) +
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

#Variance
data_diagnostic <- broom::augment(bic_mod)

ggplot(aes(x = .fitted, y = .resid), data = data_diagnostic) + 
  geom_point() + geom_hline(yintercept = 0) + 
  ggtitle("Fitted vs Residuals Plot",) + 
  labs(x = "Fitted Values", y = "Residuals",
       subtitle = "Original Untransformed BIC Selected Model")


# trying a log transformation of BMI ----------
full_mod2 <- lm(log(BMI) ~ ., data = prepped_data)
summary(full_mod2)

# perform best subset selection
best_subset2 <- leaps::regsubsets(log(BMI) ~ ., data = prepped_data, nvmax = 20, method = "exhaustive")
results2 <- summary(best_subset2)

# extract and plot results
n <- nrow(prepped_data)
p <- 20
results_df2 <- 
  tibble::tibble(
    predictors = 1:p,
    adj_R2 = results2$adjr2,
    bic = results2$bic, 
    aic = n*log(results2$rss/n) + (1:p)*2
  ) 

# training the bic selected model
form <- paste("log(BMI)~", paste(names(which(results2$which[which.min(results_df2$bic),-1])), collapse = "+")) |> 
  as.formula()

bic_mod2 <- lm(form, data = prepped_data)
summary(bic_mod2)

anova(full_mod2, bic_mod2)
#Diagnostics bic_mod2

# does not perform substantially better than the non-log transformed bic

#Variance
data_diagnostic <- broom::augment(bic_mod2)

ggplot(aes(x = .fitted, y = .resid), data = data_diagnostic) + 
  geom_point() + geom_hline(yintercept = 0) + 
  ggtitle("Fitted vs Residuals Plot") + 
  labs(x = "Fitted Values", y = "Residuals")

#normality
ggplot(aes(sample = .resid), data = data_diagnostic) + 
  geom_qq() + 
  geom_qq_line() + 
  ggtitle("QQ Plot")


# trying a log transformation of BMI ----------
# trying to lessen the heteroskedacicity
recipe3 <- full_rec |> 
  recipes::step_BoxCox(BMI)

prepped_data3 <- recipes::prep(recipe3) |> recipes::bake(data)

full_mod3 <- lm(BMI ~ ., data = prepped_data3)
summary(full_mod3)

# perform best subset selection
best_subset3 <- leaps::regsubsets(BMI ~ ., data = prepped_data3, nvmax = 20, method = "exhaustive")
results3 <- summary(best_subset3)

# extract and plot results
n <- nrow(prepped_data3)
p <- 20
results_df3 <- 
  tibble::tibble(
    predictors = 1:p,
    adj_R2 = results3$adjr3,
    bic = results3$bic, 
    aic = n*log(results3$rss/n) + (1:p)*2
  ) 

# training the bic selected model
form <- paste("BMI~", paste(names(which(results3$which[which.min(results_df3$bic),-1])), collapse = "+")) |> 
  as.formula()

bic_mod3 <- lm(form, data = prepped_data3)
summary(bic_mod3)
BIC(bic_mod3)

data_diagnostic <- broom::augment(bic_mod3)

ggplot(aes(x = .fitted, y = .resid), data = data_diagnostic) + 
  geom_point() + geom_hline(yintercept = 0) + 
  ggtitle("Fitted vs Residuals Plot") + 
  labs(x = "Fitted Values", y = "Residuals")

#normality
ggplot(aes(sample = .resid), data = data_diagnostic) + 
  geom_qq() + 
  geom_qq_line() + 
  ggtitle("QQ Plot")



## plotting bics from the three models

dplyr::bind_rows(results_df, results_df2, results_df3, .id = "Transform") |> 
  dplyr::mutate(Transform = 
                  dplyr::case_when(
                    Transform == 1 ~ "No Transformation",
                    Transform == 2 ~ "log(BMI)",
                    Transform == 3 ~ "BMI BoxCox", 
                    FALSE ~ NA
                  )
                  ) |> 
  ggplot(aes(predictors, bic, color = Transform)) + 
  geom_point() + 
  geom_point(data = results_df2[which.min(results_df2$bic), ], color="darkgreen", size = 3) + 
  labs(title = "Figure 1: BIC vs. Number of Predictors", 
       x = "# Predictors", y = "BIC") + 
  theme(legend.position = "top", 
        legend.title = element_blank())


save(bic_mod2, file = "data/final_model.rds")

