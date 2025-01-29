install.packages("haven")

library(haven)

setwd("F:/LPI lessons/PerPro/elderHealth/CLHLS_2018_cross_sectional_dataset_15874")

df <- read_sav("clhls_2018_cross_sectional_dataset_15874.sav")
head(df)
View(df)

# data processing

library(dplyr)

selected_columns <- df %>%
  select(1:13, starts_with("c"), starts_with("d"))

View(selected_columns)

die_ls_columns_to_select <- c("id", "a1", "c16","c21a", "c21b","c21c", "c22", "c31a", "c31b", "c31c", "c31d", "c31e" , "c32", "c41a", "c41b", "c41c", 
                       "d1","d4meat2","d4fish2", "d4egg2", "d4suga2", "d4garl2","d4milk1","d4nut1", "d4vit1", "d4tea2",
                       "d71","d72","d81", "d82", "d91","d92","d11a", "d11c", "d11d", "d11e", "d11f", "d11g", "d11h", "d12")


die_ls <- selected_columns %>%
  select(all_of(die_ls_columns_to_select))
View(die_ls)

# convert labelled
labelled_cols <- sapply(die_ls, inherits, "haven_labelled")
print(names(die_ls)[labelled_cols])

die_ls <- die_ls %>%
  mutate(across(everything(), ~ if (inherits(., "haven_labelled")) as.numeric(.) else .))


# missing value
calc_missing_rate <- function(data, column, na_values = NA) {

  missing_count <- sum(data[[column]] %in% na_values | is.na(data[[column]]))
  
  total_count <- nrow(data)
  
  return(missing_count / total_count)
}

missing_rate_c21 <- sapply(c("c21a", "c21b", "c21c", "c22", "c31a", "c31b", "c31c","c31d", "c31e", "c41a", "c41b", "c41c"), 
                           function(col) calc_missing_rate(die_ls, col, c(9, NA)))
missing_rate_c21_df <- data.frame(variable = names(missing_rate_c21), missing_rate = missing_rate_c21)

missing_rate_c32 <- calc_missing_rate(die_ls, "c32", NA)
missing_rate_c32_df <- data.frame(variable = "c32", missing_rate = missing_rate_c32)

missing_rate_c16 <- calc_missing_rate(die_ls, "c16", c(99, NA))
missing_rate_c16_df <- data.frame(variable = "c16", missing_rate = missing_rate_c16)

missing_rate_d1 <- sapply(c("d1", "d4meat2", "d4fish2", "d4egg2", "d4suga2", "d4garl2", "d4milk1", "d4nut1", "d4vit1", "d4tea2",
                             "d71", "d72", "d81", "d82", "d91", "d92", "d11a", "d11c", "d11d", "d11e", "d11f", "d11g", "d11h"), 
                           function(col) calc_missing_rate(die_ls, col, c(8, 9, NA)))
missing_rate_d1_df <- data.frame(variable = names(missing_rate_d1), missing_rate = missing_rate_d1)

missing_rate_d12 <- calc_missing_rate(die_ls, "d12", c(88, 99, NA))
missing_rate_d12_df <- data.frame(variable = "d12", missing_rate = missing_rate_d12)


missing_rate_summary <- bind_rows(missing_rate_c21_df, missing_rate_c32_df, missing_rate_c16_df, missing_rate_d12_df,missing_rate_d1_df)


View(missing_rate_summary)

# data processing
die_ls_cleaned <- die_ls %>%
  mutate(
    
    across(c("c21a", "c21b", "c21c", "c22", "c31a", "c31b", "c31c", "c31d", "c31e","c41a", "c41b", "c41c"), 
           ~ case_when(
             . == 9 ~ NA_real_,  
             . == 8 ~ 0,         
             TRUE ~ .  
           )),
    

    c32 = case_when(
      c32 %in% c(8, 9) ~ 0,  
      TRUE ~ c32
    ),
    
    
    c16 = case_when(
      c16 == 99 ~ NA_real_,  
      c16 == 88 ~ 0,         
      TRUE ~ c16
    ),
    

    across(c("d1", "d4meat2", "d4fish2", "d4egg2", "d4suga2", "d4garl2", "d4milk1", "d4nut1", "d4vit1", "d4tea2", 
             "d71", "d72", "d81", "d82", "d91", "d92", "d11a", "d11c", "d11d", "d11e", "d11f", "d11g", "d11h"), 
           ~ case_when(
             . %in% c(8, 9) ~ NA_real_,  
             TRUE ~ .  
           )),
    

    d12 = case_when(
      d12 == 88 ~ 0,  
      d12 == 99 ~ NA_real_,  
      TRUE ~ d12
    )
  ) %>%
  filter(

    complete.cases(.)
  )


View(die_ls_cleaned)

die_ls_cleaned <- die_ls_cleaned[, !(names(die_ls_cleaned) %in% "c22")]


# standard

# classify c16
die_ls_cleaned_log <- die_ls_cleaned %>%
  mutate(
    c16_group = case_when(
      c16 == 0 ~ "No Response",        
      c16 <= 3 ~ "Low Level",          
      c16 > 3 & c16 <= 6 ~ "Medium Level", 
      c16 > 6 ~ "High Level"           
    ),
    c16_group = factor(c16_group, levels = c("No Response", "Low Level", "Medium Level", "High Level")), 
    c16_group_score = case_when(      
      c16_group == "No Response" ~ 0,
      c16_group == "Low Level" ~ 1,
      c16_group == "Medium Level" ~ 2,
      c16_group == "High Level" ~ 3
    )
  )



# # noemalize c16_group_score
# c16_min <- min(die_ls_cleaned_log$c16_group_score, na.rm = TRUE)
# c16_max <- max(die_ls_cleaned_log$c16_group_score, na.rm = TRUE)
# 
# die_ls_cleaned_log <- die_ls_cleaned_log %>%
#   mutate(c16_normalized = (c16_group_score - c16_min) / (c16_max - c16_min))



# standardize c16_group_score 
c16_mean <- mean(die_ls_cleaned_log$c16_group_score, na.rm = TRUE)
c16_sd <- sd(die_ls_cleaned_log$c16_group_score, na.rm = TRUE)


die_ls_cleaned_log <- die_ls_cleaned_log %>%
  mutate(c16_standardized = (c16_group_score - c16_mean) / c16_sd)



# calculate cognitive score

die_ls_cleaned_log <- die_ls_cleaned_log %>%
  mutate(
    Memory_score = rowSums(select(., c21a:c21c, c41a:c41c), na.rm = TRUE), 
    Attention_score = c16_standardized,                                     
    Calculation_score = rowSums(select(., c31a:c31e), na.rm = TRUE),       
    Logic_execution_score = c32,                                           
    Cognition_score = 0.5 * Memory_score +                                 
      0.25 * Attention_score +
      0.15 * Calculation_score +
      0.1 * Logic_execution_score
  )


View(die_ls_cleaned_log)


# die_ls_cleaned_log <- die_ls_cleaned_log 
#   mutate(
#     d1 = factor(d1, levels = 1:5, labels = c("rice", "corn", "wheat", "rice_and_wheat", "other")),
#     across(starts_with("a"), ~ factor(.x, levels = c(1, 2), labels = c("male", "female"))),
#     across(starts_with("d7"), ~ factor(.x, levels = c(1, 2), labels = c("yes", "no"))),
#     across(starts_with("d8"), ~ factor(.x, levels = c(1, 2), labels = c("yes", "no"))),
#     across(starts_with("d9"), ~ factor(.x, levels = c(1, 2), labels = c("yes", "no"))),
#     across(starts_with("d4"), ~ factor(.x, levels = 1:5, ordered = TRUE)),
#     across(starts_with("d11"), ~ factor(.x, levels = 1:5, ordered = TRUE))
#   )


die_ls_cleaned_tag <- die_ls_cleaned_log
  die_ls_cleaned_tag <- die_ls_cleaned_tag %>%
    mutate(
      
      d1 = factor(d1, levels = 1:5, labels = c("rice", "corn", "wheat", "rice_and_wheat", "other")),
      
      across(starts_with("a1"), ~ factor(.x, levels = c(1, 2), labels = c("male", "female"))),
      
      across(starts_with("d7"), ~ factor(.x, levels = c(1, 2), labels = c("yes", "no"))),
      across(starts_with("d8"), ~ factor(.x, levels = c(1, 2), labels = c("yes", "no"))),
      across(starts_with("d9"), ~ factor(.x, levels = c(1, 2), labels = c("yes", "no"))),
      
      across(starts_with("d4"), ~ factor(.x, levels = 1:5, ordered = TRUE)),
      across(starts_with("d11"), ~ factor(.x, levels = 1:5, ordered = TRUE)),

    )

die_ls_cleaned_tag <- die_ls_cleaned_tag %>%
  mutate(d12_standardized = scale(d12))

die_ls_cleaned_tag <- die_ls_cleaned_tag %>%
  mutate(

    d7 = case_when(
      d71 == "no" & d72 == "no" ~ "never",
      d71 == "yes" & d72 == "yes" ~ "always",
      d71 == "no" & d72 == "yes" ~ "UsedTo",
      d71 == "yes" & d72 == "no" ~ "OnlyNow",
      TRUE ~ NA_character_  
    ),
    
    d8 = case_when(
      d81 == "no" & d82 == "no" ~ "never",
      d81 == "yes" & d82 == "yes" ~ "always",
      d81 == "no" & d82 == "yes" ~ "UsedTo",
      d81 == "yes" & d82 == "no" ~ "OnlyNow",
      TRUE ~ NA_character_
    ),
    
    d9 = case_when(
      d91 == "no" & d92 == "no" ~ "never",
      d91 == "yes" & d92 == "yes" ~ "always",
      d91 == "no" & d92 == "yes" ~ "UsedTo",
      d91 == "yes" & d92 == "no" ~ "OnlyNow",
      TRUE ~ NA_character_
    )
  )
View(die_ls_cleaned_tag)

# map frequency
frequency_weights <- c(1, 0.75, 0.5, 0.25, 0) 


die_ls_cleaned_tag_mapp <- die_ls_cleaned_tag %>%
  mutate(
    across(starts_with("d4"), ~ frequency_weights[.x]),
    across(starts_with("d11"), ~ frequency_weights[.x])
  )

View(die_ls_cleaned_tag_mapp)

# Analysis

# correlation

# # person
# 
# selected_vars <- die_ls_cleaned_log %>%
#   select(Cognition_score, a1, d1, d12, d4meat2, d4fish2, d4egg2, d4suga2, 
#          d4garl2, d4milk1, d4nut1, d4vit1, d4tea2, d71, d72, d81, d82,d91, d92, d11a, d11c, d11d, d11e, 
#          d11f, d11g, d11h)
# 

# cor_matrix <- cor(selected_vars, use = "pairwise.complete.obs")
# 

# cognition_cor <- cor_matrix["Cognition_score", -1]  # 去掉因变量本身
# 

# print(cognition_cor)

# spearman


library(dplyr)
# install.packages("corrplot")
library(corrplot)


selected_vars_spear <- die_ls_cleaned_log %>%
  select(Cognition_score, a1, d1, d12, d4meat2, d4fish2, d4egg2, d4suga2, 
         d4garl2, d4milk1, d4nut1, d4vit1, d4tea2, d71, d72, d81, d82,d91, d92, d11a, d11c, d11d, d11e, 
         d11f, d11g, d11h)


spearman_cor_matrix <- cor(selected_vars, use = "pairwise.complete.obs", method = "spearman")

# correlation
spearman_cognition_cor <- spearman_cor_matrix["Cognition_score", -1]  


print(spearman_cognition_cor)


# distribution

hist(die_ls_cleaned_log_mapp$Cognition_score, breaks = 20, main = "Cognition Score Distribution", xlab = "Cognition Score")
qqnorm(die_ls_cleaned_log_mapp$Cognition_score)
qqline(die_ls_cleaned_log_mapp$Cognition_score, col = "red")




# gaussian

# model_cognition_gaussian <- glm(
#   Cognition_score ~ d1 + d12_standardized + 
#     d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + d4vit1 + d4tea2 + 
#     d71 + d72 + d81 + d82 + d91 + d92 + 
#     d11a + d11c + d11d + d11e + d11f + d11g + d11h,
#   data = die_ls_cleaned_log_mapp,
#   family = gaussian(link = "identity") 
# )
# 
# plot(model_cognition_gaussian, which = 1)  
# plot(model_cognition_gaussian, which = 2)  
# 
# model_cognition_poly <- glm(
#   Cognition_score ~ d1 + poly(d12_standardized, 2) + 
#     d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + d4vit1 + d4tea2 + 
#     d71 + d72 + d81 + d82 + d91 + d92 + 
#     d11a + d11c + d11d + d11e + d11f + d11g + d11h,
#   data = die_ls_cleaned_log_mapp,
#   family = gaussian(link = "identity")
# )
# 
# plot(model_cognition_poly, which = 1)  
# plot(model_cognition_poly, which = 2)  




# log cognitive

die_ls_cleaned_tag_mapp_log <- die_ls_cleaned_tag_mapp %>%
  mutate(Cognition_score_log = log(Cognition_score + 1))


# gaussian
# model_cognition_log <- glm(
#   Cognition_score_log ~ d1 + poly(d12_standardized, 2) + 
#     d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + d4vit1 + d4tea2 + 
#     d71 + d72 + d81 + d82 + d91 + d92 + 
#     d11a + d11c + d11d + d11e + d11f + d11g + d11h,
#   data = die_ls_cleaned_log_mapp_log,
#   family = gaussian(link = "identity")
# )
# 
# plot(model_cognition_log, which = 1)  
# plot(model_cognition_log, which = 2)  
# 

# fitted_values <- fitted(model_cognition_log)
# 

# residuals <- residuals(model_cognition_log, type = "deviance")
# 
# par(mfrow = c(1, 1))

# plot(fitted_values, residuals, 
#      xlab = "Fitted Values", 
#      ylab = "Residuals", 
#      main = "Residuals vs Fitted with LOESS")
# abline(h = 0, col = "red", lwd = 2)  
# 

# lines(loess.smooth(fitted_values, residuals), col = "blue", lwd = 2)


# install.packages("quantreg")


# install.packages("installr")
# library(installr)
# updateR()


# Sys.setenv(PATH = paste("D:/R/Rtools/rtools44/usr/bin", Sys.getenv("PATH"), sep=";"))
# Sys.which("make")

# options(repos = c(CRAN = "https://cran.r-project.org"))
# install.packages("Matrix")
# 
# install.packages("SparseM")


library(SparseM)
library(quantreg)
library(ggplot2)

# quantile regression（25%/50%/75% ）

quantile_model_50 <- rq(Cognition_score ~ a1 + d1 + d12_standardized + 
                       d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
                       d4vit1 + d4tea2 + d7 + d8 + d9 + 
                       d11a + d11c + d11d + d11e + d11f + d11g + d11h, 
                     data = die_ls_cleaned_tag_mapp, tau = 0.5)

quantile_model_25 <- rq(Cognition_score ~ a1 + d1 + d12_standardized + 
                          d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
                          d4vit1 + d4tea2 + d7 + d8 + d9 +  
                          d11a + d11c + d11d + d11e + d11f + d11g + d11h, 
                        data = die_ls_cleaned_tag_mapp, tau = 0.25)

quantile_model_75 <- rq(Cognition_score ~ a1 + d1 + d12_standardized + 
                          d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
                          d4vit1 + d4tea2 + d7 + d8 + d9 +  
                          d11a + d11c + d11d + d11e + d11f + d11g + d11h, 
                        data = die_ls_cleaned_tag_mapp, tau = 0.75)


summary(quantile_model_25)
summary(quantile_model_50)
summary(quantile_model_75)

# install.packages("broom")
library(broom)

# coefficient
coefficients_data_50 <- tidy(quantile_model_50, conf.int = TRUE, conf.level = 0.95)

# extract variables
significant_coefficients <- coefficients_data_50 %>%
  filter(p.value < 0.05)
coefficients_data_50_filtered <- coefficients_data_50 %>%
  filter(term != "(Intercept)")

ggplot(coefficients_data_50_filtered, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +
  coord_flip() +
  labs(
    x = "Variable",
    y = "Coefficient",
    title = "Significant Regression Coefficients (p < 0.05)"
  ) +
  theme_minimal()



# coefficient
coefficients_data_25 <- tidy(quantile_model_25, conf.int = TRUE, conf.level = 0.95)

# extract variables
significant_coefficients <- coefficients_data_25 %>%
  filter(p.value < 0.05)
coefficients_data_25_filtered <- coefficients_data_25 %>%
  filter(term != "(Intercept)")

ggplot(coefficients_data_25_filtered, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +
  coord_flip() +
  labs(
    x = "Variable",
    y = "Coefficient",
    title = "Significant Regression Coefficients (p < 0.05)"
  ) +
  theme_minimal()

# coefficient
coefficients_data_75 <- tidy(quantile_model_75, conf.int = TRUE, conf.level = 0.95)

# extract variables）
significant_coefficients <- coefficients_data_75 %>%
  filter(p.value < 0.05)
coefficients_data_75_filtered <- coefficients_data_75 %>%
  filter(term != "(Intercept)")

ggplot(coefficients_data_75_filtered, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +
  coord_flip() +
  labs(
    x = "Variable",
    y = "Coefficient",
    title = "Significant Regression Coefficients (p < 0.05)"
  ) +
  theme_minimal()



# coefficient compare

coef_25 <- coef(quantile_model_25)
coef_50 <- coef(quantile_model_50)
coef_75 <- coef(quantile_model_75)

coef_df <- data.frame(
  Variable = names(coef_25),
  Q25 = coef_25,
  Q50 = coef_50,
  Q75 = coef_75
)

# install.packages("tidyr")
library(tidyr)
coef_long <- pivot_longer(coef_df, cols = Q25:Q75, names_to = "Quantile", values_to = "Coefficient")
View(coef_long)
coef_long_filtered <- coef_long %>%
  filter(Variable != "(Intercept)")


ggplot(coef_long_filtered, aes(x = Variable, y = Coefficient, color = Quantile)) +
  geom_point(size = 3) +
  geom_line(aes(group = Variable), linetype = "solid") +
  coord_flip() +
  labs(title = "Comparison of Coefficients Across Quantiles",
       x = "Variables", y = "Coefficient") +
  theme_minimal()


# predictions

pred_25 <- fitted(quantile_model_25)
pred_50 <- fitted(quantile_model_50)
pred_75 <- fitted(quantile_model_75)
actual <- die_ls_cleaned_tag_mapp$Cognition_score


plot(pred_25, actual, col = "red", pch = 16, xlab = "Predicted Values", ylab = "Actual Values",
     main = "Predicted vs Actual Values for Different Quantiles")
points(pred_50, actual, col = "blue", pch = 16)
points(pred_75, actual, col = "green", pch = 16)
abline(a = 0, b = 1, col = "black", lty = 2)
legend("bottomright", legend = c("tau = 0.25", "tau = 0.5", "tau = 0.75"), 
       col = c("red", "blue", "green"), pch = 16)

# TSS
tss <- sum((actual - mean(actual))^2)

# SSR
ssr_25 <- sum((actual - pred_25)^2)
ssr_50 <- sum((actual - pred_50)^2)
ssr_75 <- sum((actual - pred_75)^2)

# R²
r2_25 <- 1 - (ssr_25 / tss)
r2_50 <- 1 - (ssr_50 / tss)
r2_75 <- 1 - (ssr_75 / tss)

cat("Pseudo R² (tau = 0.25):", r2_25, "\n")
cat("Pseudo R² (tau = 0.50):", r2_50, "\n")
cat("Pseudo R² (tau = 0.75):", r2_75, "\n")

# MSE
mse_25 <- mean((actual - pred_25)^2)
mse_50 <- mean((actual - pred_50)^2)
mse_75 <- mean((actual - pred_75)^2)

cat("MSE (tau = 0.25):", mse_25, "\n")
cat("MSE (tau = 0.50):", mse_50, "\n")
cat("MSE (tau = 0.75):", mse_75, "\n")

# MAE
mae_25 <- median(abs(actual - pred_25))
mae_50 <- median(abs(actual - pred_50))
mae_75 <- median(abs(actual - pred_75))

cat("MAE (tau = 0.25):", mae_25, "\n")
cat("MAE (tau = 0.50):", mae_50, "\n")
cat("MAE (tau = 0.75):", mae_75, "\n")


performance_metrics <- data.frame(
  Tau = c(0.25, 0.50, 0.75),
  Pseudo_R2 = c(r2_25, r2_50, r2_75),
  MSE = c(mse_25, mse_50, mse_75),
  MAE = c(mae_25, mae_50, mae_75)
)

print(performance_metrics)



# coef_quantile_50 <- summary(quantile_model_50, se = "boot")
# coef_df_50 <- as.data.frame(coef_quantile$coefficients)
# 

# library(ggplot2)
# coef_df_50$Variable <- rownames(coef_df_50)
# ggplot(coef_df_50, aes(x = reorder(Variable, Estimate), y = Estimate)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = `Lower Bd`, ymax = `Upper Bd`), width = 0.2) +
#   coord_flip() +
#   labs(title = "Regression Coefficients with Confidence Intervals",
#        x = "Variables", y = "Coefficient") +
#   theme_minimal()
# 

# residuals_quantile <- residuals(quantile_model)
# 

# plot(residuals_quantile, main = "Residuals of Quantile Regression", ylab = "Residuals")
# plot(fitted(quantile_model), residuals_quantile, main = "Residuals vs Fitted Values", 
#      xlab = "Fitted Values", ylab = "Residuals")

# qqnorm(residuals_quantile)
# qqline(residuals_quantile, col = "red")
# 
# summary(quantile_model)
# 
# AIC(model_cognition_log, quantile_model)
# 

# plot(quantile_model)

# # random forest
# 
# install.packages("randomForest")
# library(randomForest)
# 

# rf_model <- randomForest(
#   Cognition_score ~ a1 + d1 + d12_standardized + 
#     d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
#     d4vit1 + d4tea2 + d7 + d8 + d9 + 
#     d11a + d11c + d11d + d11e + d11f + d11g + d11h,
#   data = die_ls_cleaned_tag_mapp,
#   ntree = 500,  
#   mtry = sqrt(ncol(die_ls_cleaned_tag_mapp) - 1), 
#   importance = TRUE,  
#   na.action = na.omit  
# )
# 
# 
# print(rf_model)
# 

# print(importance(rf_model))
# 

# varImpPlot(rf_model, main = "Variable Importance - Random Forest")
# 
# pred_rf <- predict(rf_model, newdata = die_ls_cleaned_tag_mapp)
# 

# mse_rf <- mean((die_ls_cleaned_tag_mapp$Cognition_score - pred_rf)^2)
# print(paste("MSE:", mse_rf))
# 

# ss_total_rf <- sum((die_ls_cleaned_tag_mapp$Cognition_score - mean(die_ls_cleaned_tag_mapp$Cognition_score))^2)
# ss_residual_rf <- sum((die_ls_cleaned_tag_mapp$Cognition_score - pred_rf)^2)
# r_squared_rf <- 1 - (ss_residual_rf / ss_total_rf)
# print(paste("R²:", r_squared_rf))



library(randomForest)
# install.packages("caret")
library(caret)


# set.seed(123)
# 

# trainIndex <- createDataPartition(die_ls_cleaned_tag_mapp$Cognition_score, 
#                                   p = 0.8,  
#                                   list = FALSE, 
#                                   times = 1)
# 
# train_data <- die_ls_cleaned_tag_mapp[trainIndex, ] 
# test_data <- die_ls_cleaned_tag_mapp[-trainIndex, ]  
# 

# train_control <- trainControl(method = "cv",      
#                               number = 10,        
#                               search = "grid")    
# 

# tune_grid <- expand.grid(
#   mtry = c(2, 5, 10, 15) 
# )
# 

# rf_model_cv <- train(
#   Cognition_score ~ a1 + d1 + d12_standardized + 
#     d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
#     d4vit1 + d4tea2 + d7 + d8 + d9 + 
#     d11a + d11c + d11d + d11e + d11f + d11g + d11h,
#   data = train_data,
#   method = "rf",                
#   trControl = train_control,   
#   tuneGrid = tune_grid,         
#   ntree = 1000,                  
#   na.action = na.omit           
# )
# 
# 
# print(rf_model_cv)
# 

# print(importance(rf_model_2))
# 
# 
# varImpPlot(rf_model_2, main = "Variable Importance - Random Forest")
# 

# pred_rf <- predict(rf_model_cv, newdata = test_data)
# 
）
# mse_rf <- mean((test_data$Cognition_score - pred_rf)^2)
# print(paste("MSE:", mse_rf))
# 

# ss_total_rf <- sum((test_data$Cognition_score - mean(test_data$Cognition_score))^2)
# ss_residual_rf <- sum((test_data$Cognition_score - pred_rf)^2)
# r_squared_rf <- 1 - (ss_residual_rf / ss_total_rf)
# print(paste("R²:", r_squared_rf))
# 
# 
# # GBM
# 

# tune_grid_gbm <- expand.grid(
#   n.trees = c(100, 500, 1000),     # 树的数量
#   interaction.depth = c(1, 3, 5),  # 每棵树的最大深度
#   shrinkage = c(0.01, 0.1),        # 学习率
#   n.minobsinnode = c(10, 20)       # 叶节点的最小样本数
# )
# 

# gbm_model <- train(
#   Cognition_score ~ a1 + d1 + d12_standardized + 
#     d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
#     d4vit1 + d4tea2 + d7 + d8 + d9 + 
#     d11a + d11c + d11d + d11e + d11f + d11g + d11h,
#   data = train_data,
#   method = "gbm",                 # 使用 GBM 方法
#   trControl = train_control,      # 使用交叉验证控制参数
#   tuneGrid = tune_grid_gbm,           # 网格搜索调优参数
#   verbose = FALSE                 # 禁止 GBM 输出冗长信息
# )
# 

# print(gbm_model)
# 

# test_pred_gbm <- predict(gbm_model, newdata = test_data)
# mse_gbm <- mean((test_pred_gbm - test_data$Cognition_score)^2)
# r_squared_gbm <- 1 - sum((test_data$Cognition_score - test_pred_gbm)^2) /
#   sum((test_data$Cognition_score - mean(test_data$Cognition_score))^2)
# 
# print(paste("Test MSE:", mse_gbm))
# print(paste("Test R²:", r_squared_gbm))
# 
# 
# 
# # bayesian
# 
# install.packages("rstanarm")
# install.packages("Rcpp")
# library(Rcpp)
# 
# library(rstanarm)
# 
# bayesian_model <- stan_glm(Cognition_score_log ~ d1 + poly(d12_standardized, 2) + 
#                              d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
#                              d4vit1 + d4tea2 + d71 + d72 + d81 + d82 + d91 + d92 + 
#                              d11a + d11c + d11d + d11e + d11f + d11g + d11h, 
#                            data = die_ls_cleaned_log_mapp_log, 
#                            family = gaussian(), 
#                            prior = normal(0, 1), 
#                            prior_intercept = normal(0, 5))
# 
# residuals_bayesian <- residuals(bayesian_model)
# plot(fitted(bayesian_model), residuals_bayesian, main = "Residuals vs Fitted Values",
#      xlab = "Fitted Values", ylab = "Residuals")
# qqnorm(residuals_bayesian)
# qqline(residuals_bayesian, col = "red")
# 
# traceplot(bayesian_model)
# 
# summary(bayesian_model)
# 
# AIC(bayesian_model, quantile_model)
# 
# log_lik_bayesian <- log_lik(bayesian_model)
# n <- length(log_lik_bayesian)
# k <- length(coef(bayesian_model))  
# AIC_bayesian <- -2 * sum(log_lik_bayesian) + 2 * k
# AIC_quantile <- AIC(quantile_model)
# cat("AIC for Bayesian model: ", AIC_bayesian, "\n")
# cat("AIC for Quantile Regression model: ", AIC_quantile, "\n")




# # Gamma
# library(mgcv)
# 
# model_cognition_gam <- gam(
#   Cognition_score_log ~ d12_standardized + 
#     d1 + 
#     d4meat2 + d4fish2 + d4egg2 + d4suga2 + 
#     d4garl2 + d4milk1 + d4nut1 + d4vit1 + d4tea2 +
#     d71 + d72 + d81 + d82 + d91 + d92 + 
#     d11a + d11c + d11d + d11e + d11f + d11g + d11h,
#   data = die_ls_cleaned_log_mapp_log,
#   method = "REML"
# )
# 
# par(mfrow = c(2, 2))
# gam.check(model_cognition_gam)
# 
# plot(model_cognition_gam, pages = 1, residuals = TRUE)
# qqnorm(residuals(model_cognition_gam, type = "deviance"))
# qqline(residuals(model_cognition_gam, type = "deviance"), col = "red")



# model_cognition <- glm(
#   Cognition_score ~ D1 + D4meat2 + D4fish2 + D4egg2 + D4suga2 + D4garl2 + D4milk1 + D4nut1 + D4vit1 + D4tea2,
#   data = die_ls_cleaned,
#   family = binomial(link = "logit")
# )
# 

# summary(model_cognition)




# cluster

# subscore
cognition_subscores <- die_ls_cleaned_tag_mapp %>%
  select(Memory_score, Attention_score, Calculation_score, Logic_execution_score)

# standardize
cognition_scaled <- scale(cognition_subscores)


View(cognition_scaled)

View(die_ls_cleaned_tag_mapp)

# install.packages("factoextra")
library(factoextra)

# # kmeans
# fviz_nbclust(cognition_scaled, kmeans, method = "wss") + 
#   labs(title = "Elbow Method for Optimal k")
# 
# set.seed(123)

# kmeans_model <- kmeans(cognition_scaled, centers = k, nstart = 25)
# 
# die_ls_cleaned_tag_mapp$cluster_k <- as.factor(kmeans_model$cluster)
# 
# table(die_ls_cleaned_tag_mapp$cluster_k)
# 
# fviz_cluster(kmeans_model, data = cognition_scaled, 
#              geom = "point", ellipse.type = "norm") +
#   labs(title = "K-means Clustering Visualization")


# tense


library(factoextra)
# install.packages("dbscan")
library(dbscan)

# DBSCAN 


kNNdistplot(cognition_scaled, k = 30) 
abline(h = 1.0, col = "red", lty = 2) 

set.seed(123) 
dbscan_model <- dbscan(cognition_scaled, eps = 1.2, minPts = 30)


print(dbscan_model)

# visualization
fviz_cluster(list(data = cognition_scaled, cluster = dbscan_model$cluster),
             geom = "point", ellipse.type = "norm") +
  labs(title = "DBSCAN Clustering Visualization")


# add cluster to dataset
die_ls_cleaned_tag_mapp$cluster <- as.factor(dbscan_model$cluster)


die_ls_cleaned_tag_mapp_noi <- die_ls_cleaned_tag_mapp[die_ls_cleaned_tag_mapp$cluster != 0, ]
die_ls_cleaned_tag_mapp_noi$cluster <- droplevels(die_ls_cleaned_tag_mapp_noi$cluster)



View(die_ls_cleaned_tag_mapp_noi)

# cluster feature

ggplot(die_ls_cleaned_tag_mapp_noi, aes(x = as.factor(cluster), y = Memory_score, fill = as.factor(cluster))) +
  geom_boxplot() +
  labs(title = "Memory Score by Cluster", x = "Cluster", y = "Memory Score") +
  theme_minimal()

ggplot(die_ls_cleaned_tag_mapp_noi, aes(x = as.factor(cluster), y = c16, fill = as.factor(cluster))) +
  geom_boxplot() +
  labs(title = "Attention Score by Cluster", x = "Cluster", y = "Attention Score") +
  theme_minimal()

ggplot(die_ls_cleaned_tag_mapp_noi, aes(x = as.factor(cluster), y = Calculation_score, fill = as.factor(cluster))) +
  geom_boxplot() +
  labs(title = "Calculation Score by Cluster", x = "Cluster", y = "Calculation Score") +
  theme_minimal()

ggplot(die_ls_cleaned_tag_mapp_noi, aes(x = as.factor(cluster), y = Logic_execution_score, fill = as.factor(cluster))) +
  geom_boxplot() +
  labs(title = "Logic Execution Score by Cluster", x = "Cluster", y = "Logic Execution Score") +
  theme_minimal()


# Random forest

set.seed(123)
trainIndex <- createDataPartition(die_ls_cleaned_tag_mapp_noi$cluster, 
                                  p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

train_data <- die_ls_cleaned_tag_mapp_noi[trainIndex, ]  
test_data <- die_ls_cleaned_tag_mapp_noi[-trainIndex, ]  


train_control <- trainControl(method = "cv", number = 10, search = "grid")


tune_grid <- expand.grid(
  mtry = c(2, 5, 10, 15)  # 尝试不同的 mtry 值
)


rf_model_cluster <- train(
  cluster ~ a1 + d1 + d12_standardized + 
    d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
    d4vit1 + d4tea2 + d7 + d8 + d9 + 
    d11a + d11c + d11d + d11e + d11f + d11g + d11h,
  data = train_data,
  method = "rf",                
  trControl = train_control,    
  tuneGrid = tune_grid,         
  ntree = 1000,                 
  na.action = na.omit           
)


predicted_clusters <- predict(rf_model_cluster, newdata = test_data)


confusion_matrix <- confusionMatrix(predicted_clusters, test_data$cluster)

print(confusion_matrix)


accuracy <- sum(predicted_clusters == test_data$cluster) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")

# recall
conf_matrix <- confusionMatrix(predicted_clusters, test_data$cluster)
recall <- conf_matrix$byClass['Recall']
cat("Recall:", recall, "\n")

# precision
precision <- conf_matrix$byClass['Pos Pred Value'] 
cat("Precision: \n")
print(precision)

# F1 
f1_score <- 2 * (conf_matrix$byClass['Recall'] * precision) / (conf_matrix$byClass['Recall'] + precision)
cat("F1 Score: \n")
print(f1_score)


metrics <- data.frame(
  Metric = c("Accuracy", "Recall", "Precision", "F1 Score", "AUC"),
  Value = c(accuracy, recall, mean(precision), mean(f1_score), auc_value) 
)


ggplot(metrics, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.3, size = 5) +  # 添加文本标签
  labs(title = "Model Evaluation Metrics", y = "Score", x = "Metric") +
  theme_minimal()


library(pROC)


predicted_probs <- predict(rf_model_cluster, newdata = test_data, type = "prob")

roc_curve <- roc(test_data$cluster, predicted_probs[, 2])  # 假设第二列为正类的概率

auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), bty = "n")

# featue importance
importance_values <- varImp(rf_model_cluster)

ggplot(importance_values, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance", x = "Feature", y = "Importance")



# sub-cognitive analysis

# logic score

die_ls_cleaned_tag_mapp$Logic_score <- as.factor(die_ls_cleaned_tag_mapp$Logic_execution_score)
levels(die_ls_cleaned_tag_mapp$Logic_score) <- make.names(levels(die_ls_cleaned_tag_mapp$Logic_score))

View(die_ls_cleaned_tag_mapp)

set.seed(123)  
trainIndex <- createDataPartition(die_ls_cleaned_tag_mapp$Logic_score, 
                                  p = 0.8,  
                                  list = FALSE, 
                                  times = 1)

train_data <- die_ls_cleaned_tag_mapp[trainIndex, ] 
test_data <- die_ls_cleaned_tag_mapp[-trainIndex, ]  


train_control <- trainControl(method = "cv", number = 10, search = "grid", 
                              classProbs = TRUE, summaryFunction = twoClassSummary)


tune_grid <- expand.grid(mtry = c(2, 5, 10, 15)) 


rf_model_logic <- train(
  Logic_score ~ a1 + d1 + d12_standardized + 
    d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
    d4vit1 + d4tea2 + d7 + d8 + d9 + 
    d11a + d11c + d11d + d11e + d11f + d11g + d11h,
  data = train_data,
  method = "rf",                
  trControl = train_control,    
  tuneGrid = tune_grid,         
  ntree = 1000,                 
  na.action = na.omit,          
  metric = "ROC"                
)


predictions_logic <- predict(rf_model_logic, test_data)

conf_matrix_logic <- confusionMatrix(predictions_logic, test_data$Logic_score)
print(conf_matrix_logic)


accuracy <- sum(predictions_logic == test_data$Logic_score) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")


recall <- conf_matrix_logic$byClass['Recall']
precision <- conf_matrix_logic$byClass['Pos Pred Value']


f1_score <- 2 * (recall * precision) / (recall + precision)


cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1_score, "\n")


predicted_probs_logic <- predict(rf_model_logic, newdata = test_data, type = "prob")


roc_curve_logic <- roc(test_data$Logic_score, predicted_probs_logic[, 2])  

auc_value_logic <- auc(roc_curve_logic)
cat("AUC:", auc_value_logic, "\n")


metrics <- data.frame(
  Metric = c("Accuracy", "Recall", "Precision", "F1 Score", "AUC"),
  Value = c(accuracy, recall, precision, f1_score, auc_value_logic)
)

ggplot(metrics, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.3, size = 5) +  
  labs(title = "Model Evaluation Metrics (Including AUC)", y = "Score", x = "Metric") +
  theme_minimal()


plot(roc_curve_logic, main = "ROC Curve for Logic Execution Model", col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value_logic, 2)), bty = "n")


importance_values_logic <- varImp(rf_model_logic, scale = TRUE)

importance_data <- importance_values_logic$importance
importance_data <- importance_data[order(-importance_data$Overall), , drop = FALSE]  
importance_data$Feature <- rownames(importance_data)

ggplot(importance_data, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance for Logic Execution Model", x = "Feature", y = "Importance") +
  theme_minimal()



# attention

die_ls_cleaned_tag_mapp$Attention_score_name <- as.factor(die_ls_cleaned_tag_mapp$c16_group)
levels(die_ls_cleaned_tag_mapp$Attention_score_name) <- make.names(levels(die_ls_cleaned_tag_mapp$Attention_score_name))

View(die_ls_cleaned_tag_mapp)

set.seed(123)  
trainIndex <- createDataPartition(die_ls_cleaned_tag_mapp$Attention_score_name, 
                                  p = 0.8,  
                                  list = FALSE, 
                                  times = 1)

train_data <- die_ls_cleaned_tag_mapp[trainIndex, ]  
test_data <- die_ls_cleaned_tag_mapp[-trainIndex, ]  

train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE 
)


tune_grid <- expand.grid(mtry = c(2, 5, 10, 15))  


rf_model_attention <- train(
  Attention_score_name ~ a1 + d1 + d12_standardized + 
    d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
    d4vit1 + d4tea2 + d7 + d8 + d9 + 
    d11a + d11c + d11d + d11e + d11f + d11g + d11h,
  data = train_data,
  method = "rf",                
  trControl = train_control,    
  tuneGrid = tune_grid,         
  ntree = 1000,                 
  na.action = na.omit,          
  metric = "Accuracy"           
)

predictions_attention <- predict(rf_model_attention, test_data)


conf_matrix_attention <- confusionMatrix(predictions_attention, test_data$Attention_score_name)
print(conf_matrix_attention)


accuracy <- conf_matrix_attention$overall['Accuracy']
recall <- conf_matrix_attention$byClass['Recall']  
precision <- conf_matrix_attention$byClass['Pos Pred Value']  
f1_score <- 2 * (recall * precision) / (recall + precision)  


cat("Accuracy:", accuracy, "\n")
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1_score, "\n")

importance_values <- varImp(rf_model_attention, scale = FALSE)

importance_df <- as.data.frame(importance_values$importance)
importance_df$Variable <- rownames(importance_df)
rownames(importance_df) <- NULL


ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  
  labs(
    title = "Variable Importance",
    x = "Features",
    y = "Importance Score"
  ) +
  theme_minimal()


# memory

die_ls_cleaned_tag_mapp$Memory_score_class <- cut(
  die_ls_cleaned_tag_mapp$Memory_score,
  breaks = c(-1, 2, 4, 6),  
  labels = c("Low", "Medium", "High"), 
  right = TRUE  
)


table(die_ls_cleaned_tag_mapp$Memory_score_class)


set.seed(123)
trainIndex <- createDataPartition(die_ls_cleaned_tag_mapp$Memory_score_class, 
                                  p = 0.8, 
                                  list = FALSE)
train_data <- die_ls_cleaned_tag_mapp[trainIndex, ]
test_data <- die_ls_cleaned_tag_mapp[-trainIndex, ]


train_control <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE,  
)

tune_grid <- expand.grid(mtry = c(2, 5, 10, 15))

rf_model_memory <- train(
  Memory_score_class ~ a1 + d1 + d12_standardized + 
    d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
    d4vit1 + d4tea2 + d7 + d8 + d9 + 
    d11a + d11c + d11d + d11e + d11f + d11g + d11h,
  data = train_data,
  method = "rf",                
  trControl = train_control,    
  tuneGrid = tune_grid,         
  ntree = 1000,                 
  metric = "Accuracy",          
  na.action = na.omit           
)


predictions_memory <- predict(rf_model_memory, newdata = test_data)


confusion_memory <- confusionMatrix(predictions_memory, test_data$Memory_score_class)
print(confusion_memory)

accuracy <- confusion_memory$overall['Accuracy']
recall <- confusion_memory$byClass['Sensitivity']
precision <- confusion_memory$byClass['Pos Pred Value']
f1_score <- 2 * (precision * recall) / (precision + recall)


cat("Accuracy:", accuracy, "\n")
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1_score, "\n")


importance_values <- varImp(rf_model_memory, scale = FALSE)

importance_df <- as.data.frame(importance_values$importance)
importance_df$Variable <- rownames(importance_df)
rownames(importance_df) <- NULL


ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + 
  labs(
    title = "Variable Importance",
    x = "Features",
    y = "Importance Score"
  ) +
  theme_minimal()


# calculation


die_ls_cleaned_tag_mapp$Calculation_score_class <- cut(
  die_ls_cleaned_tag_mapp$Calculation_score,
  breaks = c(-1, 2, 4, 5),  
  labels = c("Low", "Medium", "High"),  
  right = TRUE 
)


table(die_ls_cleaned_tag_mapp$Calculation_score_class)


set.seed(123)
trainIndex <- createDataPartition(die_ls_cleaned_tag_mapp$Calculation_score_class, 
                                  p = 0.8, 
                                  list = FALSE)
train_data <- die_ls_cleaned_tag_mapp[trainIndex, ]
test_data <- die_ls_cleaned_tag_mapp[-trainIndex, ]


train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE)
tune_grid <- expand.grid(mtry = c(2, 5, 10, 15))


rf_model_calculation <- train(
  Calculation_score_class ~ a1 + d1 + d12_standardized + 
    d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
    d4vit1 + d4tea2 + d7 + d8 + d9 + 
    d11a + d11c + d11d + d11e + d11f + d11g + d11h,
  data = train_data,
  method = "rf",
  trControl = train_control,
  tuneGrid = tune_grid,
  ntree = 1000,
  metric = "Accuracy",
  na.action = na.omit
)


predictions_calculation <- predict(rf_model_calculation, newdata = test_data)


conf_matrix_calculation <- confusionMatrix(predictions_calculation, test_data$Calculation_score_class)
print(conf_matrix_calculation)


accuracy <- conf_matrix_calculation$overall["Accuracy"]
precision <- conf_matrix_calculation$byClass["Pos Pred Value"] 
recall <- conf_matrix_calculation$byClass["Sensitivity"]        
f1_score <- 2 * (precision * recall) / (precision + recall)  


cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")


importance_values <- varImp(rf_model_calculation, scale = FALSE)


importance_df <- as.data.frame(importance_values$importance)
importance_df$Variable <- rownames(importance_df)
rownames(importance_df) <- NULL


ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + 
  labs(
    title = "Variable Importance for Calculation Score",
    x = "Features",
    y = "Importance Score"
  ) +
  theme_minimal()




# # SVM
# 

# library(e1071)  
# library(caret)  
# 

# set.seed(123)
# trainIndex <- createDataPartition(die_ls_cleaned_tag_mapp_noi$cluster, 
#                                   p = 0.8, 
#                                   list = FALSE, 
#                                   times = 1)
# 
# train_data <- die_ls_cleaned_tag_mapp_noi[trainIndex, ]  
# test_data <- die_ls_cleaned_tag_mapp_noi[-trainIndex, ]
# 

# train_control <- trainControl(method = "cv", number = 10, search = "grid")
# 

# tune_grid <- expand.grid(
#   sigma = c(0.1, 0.5, 1, 2),  
#   C = c(0.1, 1, 10) 
# )
# 

# svm_model_cluster <- train(
#   cluster ~ a1 + d1 + d12_standardized + 
#     d4meat2 + d4fish2 + d4egg2 + d4suga2 + d4garl2 + d4milk1 + d4nut1 + 
#     d4vit1 + d4tea2 + d7 + d8 + d9 + 
#     d11a + d11c + d11d + d11e + d11f + d11g + d11h,
#   data = train_data,
#   method = "svmRadial",           
#   trControl = train_control,      
#   tuneGrid = tune_grid,           
#   na.action = na.omit            
# )

# predicted_clusters <- predict(svm_model_cluster, newdata = test_data)
# 
# confusion_matrix <- confusionMatrix(predicted_clusters, test_data$cluster)
# 
# print(confusion_matrix)
# 
# accuracy <- sum(predicted_clusters == test_data$cluster) / nrow(test_data)
# cat("Accuracy:", accuracy, "\n")




# # HDBSCAN
# 
# library(dbscan)
# 
# set.seed(123)
# 

# hdbscan_model <- hdbscan(cognition_scaled, minPts = 25)
# 

# print(hdbscan_model)
# 
）
# fviz_cluster(list(data = cognition_scaled, cluster = hdbscan_model$cluster),
#              geom = "point", ellipse.type = "norm") +
#   labs(title = "HDBSCAN Clustering Visualization")
# 
# 
# 

# install.packages("umap")
# library(umap)

# umap_results <- umap(cognition_scaled)

# umap_data <- as.data.frame(umap_results$layout)
# umap_data$cluster <- as.factor(hdbscan_model$cluster)
# 
# ggplot(umap_data, aes(x = V1, y = V2, color = cluster)) +
#   geom_point(alpha = 0.6, size = 1.5) +
#   labs(title = "HDBSCAN Clustering Visualization (UMAP)",
#        x = "UMAP 1", y = "UMAP 2") +
#   theme_minimal()
# 
# 
# # GMM
# 

# install.packages("mclust")
# library(mclust)

# gmm_model <- Mclust(cognition_scaled)
# 

# summary(gmm_model)
# 

# gmm_model_optimal <- Mclust(cognition_scaled, G = 2:15) 
# summary(gmm_model_optimal)

# plot(gmm_model_optimal, what = "BIC")

# cluster_numbers <- 2:15  
#
# icl_values <- sapply(cluster_numbers, function(g) {
#   model <- Mclust(cognition_scaled, G = g)
#   return(model$icl)
# })
# 

# print(icl_values)

# plot(cluster_numbers, icl_values, type = "b", pch = 19, col = "blue",
#      main = "ICL for Optimal Clustering", xlab = "Number of Clusters (G)", ylab = "ICL")
# 
# 
# fviz_cluster(gmm_model, data = cognition_scaled, geom = "point") +
#   labs(title = "GMM Clustering Visualization") +
#   theme_minimal()
# 

# umap_results <- umap(cognition_scaled, n_neighbors = 30, min_dist = 0.2)

# umap_data <- as.data.frame(umap_results$layout)
# umap_data$cluster <- as.factor(gmm_model$classification)
# 
# ggplot(umap_data, aes(x = V1, y = V2, color = cluster)) +
#   geom_point(alpha = 0.6, size = 1.5) +
#   labs(title = "GMM Clustering Visualization (Adjusted UMAP)",
#        x = "UMAP 1", y = "UMAP 2") +
#   theme_minimal()



