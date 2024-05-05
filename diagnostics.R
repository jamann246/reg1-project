#Diagnostics bic_mod
# testing BIC model 2, with a log transformed BMI
load("./data/final_model.rds")
load("./data/prepped_data.rda")

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

shapiro.test(data_diagnostic$.resid)


#Halfnorm
library(gghalfnorm)
library(faraway)
x <- data_diagnostic$.resid
gghalfnorm(x, nlab = 2, labs = as.character(seq_along(x)), repel = FALSE) + ggtitle("Half Normal Plot")

# excluding top 3 points
exclude <- prepped_data[-c(345,143),]

exc_mod <- lm(log(BMI) ~ Age + Vegetable_Intake + Physical_Activity + Screen_Time + 
                Family_History_Yes + High_Caloric_Food_Yes + Snacking_Frequently + 
                Snacking_Always + Alcohol_Consumption_Sometimes + Transportation_Type_Automobile, 
              data = exclude)

summary(exc_mod)
summary(bic_mod2)

# collinearity
x <- model.matrix(bic_mod2)[,-1] 
e <- eigen(t(x) %*% x) 
tibble::tibble(
  `Eigen Value` = e$values |> 
    round(digits=2) |> 
    format(scientific=FALSE),
  `Condition Number` = sqrt(e$val[1]/e$val) |> 
    round(digits=2) |> 
    format(scientific=FALSE)
)

faraway::vif(x) |> 
  round(digits=2) |> 
  sort(decreasing=TRUE) |> 
  data.frame() |> 
  dplyr::rename(VIF=1) |> 
  tibble::rownames_to_column(var="Variable") 

corrplot::corrplot(
  cor(x), 
  type = "lower"
)

# independence of errors
# the corrplot indicates some potential collinearity with age and transportaiton, I'll do a quick graph

data_diagnostic |> 
  ggplot(aes(Age, .resid)) + 
  geom_point() + 
  facet_wrap(~factor(Transportation_Type_Automobile), ncol = 1) + 
  geom_smooth(method = "lm") + 
  labs(title = "Model Residuals vs. Age by People Who Use Cars")

data_diagnostic |> 
  ggplot(aes(factor(Transportation_Type_Automobile), .resid)) + 
  geom_boxplot() + 
  labs(title = "Model Residuals vs. Auto Transportation")
