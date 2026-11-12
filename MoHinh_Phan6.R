if (!requireNamespace("randomForest", quietly = TRUE)) {
  stop('Cần cài gói randomForest bằng install.packages("randomForest").')
}

input_file <- "GPU_power_truoc_trung_vi.csv"
if (!file.exists(input_file)) {
  stop("Hãy chạy NewProject.R trước để tạo ", input_file, ".")
}

predictor_num <- c("Core_Speed", "Memory_Bandwidth", "Memory", "TMUs", "Process")
reg_formula <- Max_Power ~ Core_Speed + Memory_Bandwidth + Memory +
  TMUs + Process + Manufacturer + Notebook_GPU
prediction_data <- read.csv(input_file, stringsAsFactors = FALSE)
prediction_data <- prediction_data[, all.vars(reg_formula), drop = FALSE]
prediction_data$Manufacturer <- factor(
  prediction_data$Manufacturer, levels = c("AMD", "ATI", "Intel", "Nvidia")
)
prediction_data$Notebook_GPU <- factor(prediction_data$Notebook_GPU, levels = c("No", "Yes"))

# Tái lập phép chia theo dòng ở mục 5.2.1.
set.seed(99, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
train_index <- sample.int(nrow(prediction_data), size = floor(0.7 * nrow(prediction_data)))
train_raw <- prediction_data[train_index, , drop = FALSE]
test_raw <- prediction_data[-train_index, , drop = FALSE]

# Chỉ dùng tập huấn luyện để tính các trung vị điền khuyết.
train_by_manufacturer <- split(train_raw, train_raw$Manufacturer, drop = TRUE)
median_train <- lapply(predictor_num, function(variable) {
  vapply(train_by_manufacturer, function(group) median(group[[variable]], na.rm = TRUE), numeric(1))
})
names(median_train) <- predictor_num
global_train <- vapply(train_raw[predictor_num], median, numeric(1), na.rm = TRUE)

fill_from_train <- function(data) {
  for (variable in predictor_num) {
    missing <- is.na(data[[variable]])
    manufacturer <- as.character(data$Manufacturer[missing])
    data[[variable]][missing] <- median_train[[variable]][manufacturer]
    data[[variable]][is.na(data[[variable]])] <- global_train[[variable]]
  }
  data
}

train_data <- fill_from_train(train_raw)
test_data <- fill_from_train(test_raw)
stopifnot(!anyNA(train_data), !anyNA(test_data))

linear_model <- lm(reg_formula, data = train_data)
set.seed(99)
forest_model <- randomForest::randomForest(
  reg_formula, data = train_data, ntree = 500, mtry = 2, nodesize = 5
)

observed <- test_data$Max_Power
linear_predictions <- predict(linear_model, newdata = test_data)
forest_predictions <- predict(forest_model, newdata = test_data)

prediction_metrics <- function(predicted) {
  error <- observed - predicted
  mse <- mean(error^2)
  c(
    MAE = mean(abs(error)),
    MSE = mse,
    RMSE = sqrt(mse),
    R2_Test = 1 - sum(error^2) / sum((observed - mean(observed))^2),
    Negative_Predictions = sum(predicted < 0)
  )
}

prediction_results <- rbind(
  Linear_Regression = prediction_metrics(linear_predictions),
  Random_Forest = prediction_metrics(forest_predictions)
)

cat(sprintf("N = %d | Train = %d | Test = %d\n",
            nrow(prediction_data), nrow(train_data), nrow(test_data)))
cat("randomForest:", as.character(utils::packageVersion("randomForest")), "\n")
print(round(prediction_results, 4))

rmse_lm <- prediction_results["Linear_Regression", "RMSE"]
rmse_rf <- prediction_results["Random_Forest", "RMSE"]
cat(sprintf("RMSE reduction (LM - RF): %.4f W (%.2f%%)\n",
            rmse_lm - rmse_rf, 100 * (rmse_lm - rmse_rf) / rmse_lm))
