library(ggplot2)
theme_set(theme_minimal())

load("./data/clean_data.rda")
load("./data/dirty_data.rda")

psych::describe(data) |> 
  dplyr::select(-c(median, trimmed, mad))

# univariate
# numeric data

# figure B
data_dirty |> 
  tidyr::pivot_longer(cols = dplyr::where(is.numeric)) |> 
  dplyr::filter(!(name %in% c("Age","BMI"))) |> 
  ggplot2::ggplot(ggplot2::aes(value, fill = name)) + 
  ggplot2::geom_histogram() + 
  ggplot2::facet_wrap(~name, scales = "free") + 
  ggplot2::labs(title = "Histograms of Integer Variables (Raw)") + 
  ggplot2::theme(legend.position = "none", 
                 axis.title = element_blank()) 

# figure C
data |> 
  tidyr::pivot_longer(cols = dplyr::where(is.numeric)) |> 
  ggplot2::ggplot(ggplot2::aes(value, fill = name)) + 
  ggplot2::geom_histogram() + 
  ggplot2::facet_wrap(~name, scales = "free") + 
  labs(title = "Histogram of Numeric Variables") + 
  ggplot2::theme(legend.position = "none", 
                 axis.title = element_blank())

# CH2O, FAF, FCVC, NCP, TUE are discrete
# Age, BMI, Height, Weight are continuous, normal or log normal distributed


# factor data
factors <- c("Alcohol_Consumption","Transportation_Type", 
             "Calorie_Monitoring", "Snacking","Smoking",
             "Family_History","High_Caloric_Food", "Gender")
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


# table 1
frequencies <- 
  purrr::map_df(factors, \(i){
    f <- data |> 
      dplyr::pull(var = i) |> 
      table() |> 
      t() |> 
      data.frame() |> 
      dplyr::mutate(
        Question = i,
        Total = sum(Freq),
        Proportion = round(Freq/sum(Freq), digits = 2)
      ) 
    mean <- data |> 
      dplyr::summarise(
        Mean_BMI = mean(BMI), 
        .by = i
      ) |> 
      tidyr::pivot_longer(i, names_to = "Question", values_to = "Var2")
    
    dplyr::left_join(f, mean)
    
  }) |> 
  dplyr::select(Question, Var2, Freq, Proportion, Mean_BMI)

tests <- 
  purrr::map_df(factors, \(i){
    q <- colnames(data[,i])
    bmi <- aov(
      formula = as.formula(paste("BMI ~ ", i)), 
      data = data
    )
    
    tibble::tibble(
      Question = i, 
      P_Value = c(summary(bmi)[[1]][["Pr(>F)"]][1])
    )
  })

analysis <-
  dplyr::left_join(
    x = frequencies, 
    y = tests
  ) 

# testing diff between bikes and motorbikes

transit <- data |> 
  dplyr::filter(Transportation_Type %in% c("Motorbike", "Bike"))

t.test(transit$BMI ~ transit$Transportation_Type) # 0.8402

# some *very* infrequent methods of transit
# smoking is not at all common

# bivariate
# Family history and BMI - cat and cont
# Transportation type and BMI - cat and cont
# Transportation and age - cat  and cont
# Screen time and age - cat and cont (Jordan)


# figure C
data |> 
  tidyr::pivot_longer(cols = c(2, 5,6,9,11,12)) |> 
  ggplot(aes(value, BMI)) + 
  geom_point() + 
  facet_wrap(~name, scales = "free") + 
  geom_smooth(method = "lm") +
  labs(x = "", 
       title = "BMI vs. Numeric Variables")
  
  

data |> 
  ggplot(aes(BMI, Family_History)) + 
  geom_boxplot()

data |> 
  ggplot(aes(Transportation_Type, BMI, fill = Transportation_Type)) + 
  geom_boxplot() + 
  theme(legend.position = "none")

data |> 
  ggplot(aes(Transportation_Type, Age, fill = Transportation_Type)) + 
  geom_boxplot() + 
  theme(legend.position = "none")

data |> 
  ggplot(aes(factor(Screen_Time), Age, fill = factor(Screen_Time))) + 
  geom_boxplot() + 
  labs(x = "Screen_Time") + 
  theme(legend.position = "none")


corrplot::corrplot(
  corr = cor(data |> dplyr::select(dplyr::where(is.numeric))), 
  method = "pie", 
  type = "upper"
  )

data |> 
  ggplot(aes(Age, BMI)) + 
  geom_point() + 
  geom_smooth(method = "lm") + # looks decent, not quite linear
  labs(title = "BMI vs. Age")
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



#Bivariate Analysis


#Screen time and BMI
screen_bmi <- boxplot(BMI ~ TUE, data = data, main = "Screen Time and BMI", xlab = "Screen Time", ylab = "BMI" )
screen_bmi_anova <- aov(BMI ~ factor(TUE), data = data)
summary(screen_bmi_anova)
tukey_screen_bmi <- TukeyHSD(screen_bmi_anova)
plot(tukey_screen_bmi)


#Family history BMI and snacking
barplot(table(data$family_history_with_overweight, data$CAEC),
        beside = T,
        legend.text = T,
        xlab = "Snacking",
        ylab = "Frequency",
        main = "Snack Frequency by Family History of Obesity")

chisq.test(data$family_history_with_overweight, data$CAEC)

#Family History and High Caloric Food Intake
barplot(table(data$family_history_with_overweight, data$FAVC),
        beside = T,
        legend.text = T,
        xlab = "High Caloric Food Intake",
        ylab = "Frequency",
        main = "High Caloric Food Intake by Family History of Obesity")

chisq.test(data$family_history_with_overweight, data$FAVC)

#Vegetable consumption and gender
barplot(table(data$Gender, data$FCVC),
        beside = T,
        legend.text = T,
        xlab = "Vegetable Consumption",
        ylab = "Frequency",
        main = "Vegetable Consumption by Gender")

chisq.test(data$Gender, data$FCVC)

#Method of transportation and High Caloric Food Intake
barplot(table(data$FAVC, data$MTRANS),
        beside = T,
        legend.text = T,
        xlab = "Method of Transportation",
        ylab = "Frequency",
        main = "Method of Transportation by High Caloric Food Intake")
fisher.test(table(data$MTRANS, data$FAVC))

#Water intake and physical activity
barplot(table(data$FAF, data$CH2O),
        beside = T,
        legend.text = T,
        xlab = "Physical Activity",
        ylab = "Frequency",
        main = "Physical Activity by Water Consumption")

chisq.test(data$CH2O, data$FAF)