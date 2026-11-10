library(dplyr)
GPU_power <- read.csv("GPU_power.csv", stringsAsFactors = FALSE)
predictor_num <- c("Core_Speed", "Memory_Bandwidth", "Memory", "TMUs", "Process")

library(car)
library(lmtest)
options(width = 90, digits = 5)
analysis_data <- as.data.frame(GPU_power)
analysis_data$Manufacturer <- factor(analysis_data$Manufacturer,
  levels = c("AMD", "ATI", "Intel", "Nvidia"))
analysis_data$Notebook_GPU <- factor(analysis_data$Notebook_GPU,
  levels = c("No", "Yes"))
anova_data <- droplevels(subset(analysis_data, Manufacturer != "ATI"))
print(table(analysis_data$Manufacturer, analysis_data$Notebook_GPU))
print(c(N_ANOVA = nrow(anova_data), N_Regression_Data = nrow(analysis_data)))
anova_groups <- anova_data %>%
  group_by(Manufacturer, Notebook_GPU) %>%
  summarise(N = n(), Mean = mean(Max_Power), SD = sd(Max_Power),
            .groups = "drop")
print(as.data.frame(anova_groups), row.names = FALSE)

cell_group <- interaction(anova_data$Manufacturer, anova_data$Notebook_GPU, drop = TRUE)
normality <- lapply(split(anova_data$Max_Power, cell_group), shapiro.test)
normality_table <- data.frame(
  Group = names(normality),
  W = sapply(normality, function(x) unname(x$statistic)),
  p_value = sapply(normality, function(x) x$p.value))
print(normality_table, row.names = FALSE)
print(leveneTest(anova_data$Max_Power, cell_group, center = median))

anova_fit <- aov(Max_Power ~ Manufacturer * Notebook_GPU, data = anova_data)
print(summary(anova_fit))
print(c(DF_Error = df.residual(anova_fit),
        MSE = deviance(anova_fit) / df.residual(anova_fit)))

tukey_results <- TukeyHSD(anova_fit,
  which = "Manufacturer:Notebook_GPU", conf.level = 0.95)
print(tukey_results)

df <- read.csv("GPU_power_truoc_trung_vi.csv", stringsAsFactors = FALSE)
set.seed(99)
idx <- sample(seq_len(nrow(df)), size = floor(0.7 * nrow(df)))
train_raw <- df[idx, ]
test_raw <- df[-idx, ]
med_grp <- train_raw %>% group_by(Manufacturer) %>%
  summarise(across(all_of(predictor_num), ~ median(.x, na.rm = TRUE)), .groups = "drop")
med_all <- sapply(train_raw[predictor_num], median, na.rm = TRUE)
fill_na <- function(d) {
  for (v in predictor_num) {
    grp_val <- med_grp[[v]][match(d$Manufacturer, med_grp$Manufacturer)]
    d[[v]] <- coalesce(d[[v]], grp_val, med_all[[v]])
  }
  d$Manufacturer <- factor(d$Manufacturer, levels = levels(analysis_data$Manufacturer))
  d$Notebook_GPU <- factor(d$Notebook_GPU, levels = levels(analysis_data$Notebook_GPU))
  d
}
train_data <- fill_na(train_raw)
test_data <- fill_na(test_raw)
print(c(N_All = nrow(df), N_Train = nrow(train_data), N_Test = nrow(test_data),
        Train_NA = sum(is.na(train_data)), Test_NA = sum(is.na(test_data))))
print(table(train_data$Manufacturer))

reg_data <- train_data
reg_formula <- Max_Power ~ Core_Speed + Memory_Bandwidth + Memory +
  TMUs + Process + Manufacturer + Notebook_GPU
reg_fit <- lm(reg_formula, data = reg_data)
print(summary(reg_fit), digits = 5)

print(confint(reg_fit, level = 0.95))

shapiro_reg <- shapiro.test(residuals(reg_fit))
bp_reg <- bptest(reg_fit)
dw_reg <- dwtest(reg_fit, alternative = "two.sided")
reg_assumptions <- data.frame(
  Test = c("Shapiro-Wilk", "Breusch-Pagan", "Durbin-Watson"),
  Statistic = c(unname(shapiro_reg$statistic), unname(bp_reg$statistic),
                unname(dw_reg$statistic)),
  p_value = c(shapiro_reg$p.value, bp_reg$p.value, dw_reg$p.value))
print(reg_assumptions, row.names = FALSE)
print(vif(reg_fit))

pred_test <- predict(reg_fit, newdata = test_data)
observed <- test_data$Max_Power
prediction_results <- data.frame(
  MAE = mean(abs(observed - pred_test)),
  MSE = mean((observed - pred_test)^2),
  RMSE = sqrt(mean((observed - pred_test)^2)),
  R2_Test = 1 - sum((observed - pred_test)^2) /
    sum((observed - mean(observed))^2))
print(prediction_results, row.names = FALSE)
print(c(Negative_Predictions = sum(pred_test < 0)))
