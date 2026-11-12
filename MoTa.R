library(GGally)

source("NewProject.R")

GPU_pre <- read.csv("GPU_power_truoc_trung_vi.csv", na.strings = c("", "NA"),
                    stringsAsFactors = FALSE)

nrow(GPU_pre) == nrow(GPU_power)
nrow(GPU_identity) == nrow(GPU_power)

external_summary

imputed <- data.frame(
  Variable  = predictor_num,
  n_imputed = colSums(GPU_imputation_flags[predictor_num]),
  pct       = round(colMeans(GPU_imputation_flags[predictor_num]) * 100, 2),
  row.names = NULL
)
imputed

bang_mo_ta <- function(d, them_n = FALSE) {
  kq <- data.frame(
    Mean     = sapply(d[num_vars], mean, na.rm = TRUE),
    SD       = sapply(d[num_vars], sd, na.rm = TRUE),
    Min      = sapply(d[num_vars], min, na.rm = TRUE),
    "Q1.25%" = unname(sapply(d[num_vars], quantile, probs = 0.25, na.rm = TRUE)),
    Median   = sapply(d[num_vars], median, na.rm = TRUE),
    "Q3.75%" = unname(sapply(d[num_vars], quantile, probs = 0.75, na.rm = TRUE)),
    Max      = sapply(d[num_vars], max, na.rm = TRUE),
    check.names = FALSE
  )
  if (them_n) kq <- cbind(n = sapply(d[num_vars], function(x) sum(!is.na(x))), kq)
  kq
}
desc_main <- bang_mo_ta(GPU_power)
desc_pre  <- bang_mo_ta(GPU_pre, them_n = TRUE)
desc_main
desc_pre

group_desc <- function(d, g) {
  d %>%
    group_by(across(all_of(g))) %>%
    summarise(
      n      = n(),
      Mean   = mean(Max_Power),
      SD     = sd(Max_Power),
      Min    = min(Max_Power),
      Q1     = quantile(Max_Power, 0.25),
      Median = median(Max_Power),
      Q3     = quantile(Max_Power, 0.75),
      Max    = max(Max_Power),
      .groups = "drop"
    )
}
group_desc(GPU_power, "Manufacturer")
group_desc(GPU_power, "Notebook_GPU")
group_desc(GPU_power, c("Manufacturer", "Notebook_GPU"))

table(GPU_power$Manufacturer, GPU_power$Notebook_GPU)
round(prop.table(table(GPU_power$Manufacturer)) * 100, 2)
round(prop.table(table(GPU_power$Notebook_GPU)) * 100, 2)

summary(GPU_power %>% select(all_of(cat_vars)) %>% mutate(across(everything(), factor)))

GPU_scope <- GPU_power %>%
  mutate(Name = GPU_identity$Name, Integrated = GPU_identity$Integrated) %>%
  filter(Manufacturer %in% c("AMD", "Nvidia"), Integrated == "No")
nrow(GPU_scope)
table(GPU_scope$Manufacturer, GPU_scope$Notebook_GPU)
group_desc(GPU_scope, c("Manufacturer", "Notebook_GPU"))

luu_hinh <- TRUE

CEX_NHAN <- 1.6
CEX_TRUC <- 1.5
CEX_TIEU <- 1.6

bat_dau <- function(ten, w = 1300, h = 700) {
  if (luu_hinh) png(paste0(ten, ".png"), width = w, height = h, res = 150)
}
ket_thuc <- function() {
  if (luu_hinh) invisible(dev.off())
}

luoi <- function(nr = 1, nc = 1, mar = c(5.5, 5.8, 1.5, 1)) {
  par(mfrow = c(nr, nc), mar = mar, mgp = c(3.6, 1, 0))
  par(cex = 1, cex.lab = CEX_NHAN, cex.axis = CEX_TRUC, cex.main = CEX_TIEU)
}

nhan <- c(
  Max_Power        = "Công suất tối đa (W)",
  Core_Speed       = "Xung nhịp lõi (MHz)",
  Memory_Bandwidth = "Băng thông bộ nhớ (GB/s)",
  Memory           = "Dung lượng bộ nhớ (MB)",
  TMUs             = "Số TMU",
  Process          = "Tiến trình (nm)"
)
mau_hang <- c(AMD = "tomato", ATI = "goldenrod1", Intel = "skyblue", Nvidia = "palegreen3")
mau_note <- c(No = "lightgrey", Yes = "steelblue")

bat_dau("hinh_barplot", 1300, 600)
luoi(1, 2)
tab_manu <- table(GPU_power$Manufacturer)
bp <- barplot(tab_manu, xlab = "Hãng", ylab = "Số GPU", col = mau_hang[names(tab_manu)],
              ylim = c(0, max(tab_manu) * 1.15), border = "black")
text(bp, tab_manu, labels = tab_manu, pos = 3, cex = 1.4)
tab_note <- table(GPU_power$Notebook_GPU)
bp <- barplot(tab_note, xlab = "Notebook GPU", ylab = "Số GPU", col = mau_note[names(tab_note)],
              ylim = c(0, max(tab_note) * 1.15), border = "black")
text(bp, tab_note, labels = tab_note, pos = 3, cex = 1.4)
par(mfrow = c(1, 1))
ket_thuc()

bat_dau("hinh_hist_max_power", 1300, 600)
luoi(1, 2)
hist(GPU_power$Max_Power, breaks = 30, col = "steelblue", border = "white",
     xlab = "Công suất tối đa (W)", ylab = "Tần số", main = "")
hist(log(GPU_power$Max_Power), breaks = 30, col = "steelblue", border = "white",
     xlab = "ln(công suất tối đa, W)", ylab = "Tần số", main = "")
par(mfrow = c(1, 1))
ket_thuc()

bat_dau("hinh_hist_cac_bien", 1500, 900)
luoi(2, 3)
for (v in num_vars) {
  hist(GPU_pre[[v]], breaks = 30, col = "steelblue", border = "white",
       xlab = nhan[[v]], ylab = "Tần số", main = "")
}
par(mfrow = c(1, 1))
ket_thuc()

bat_dau("hinh_hist_theo_notebook", 1300, 600)
luoi(1, 2, mar = c(5.5, 5.8, 3, 1))
hist(GPU_power$Max_Power[GPU_power$Notebook_GPU == "No"],
     breaks = seq(0, 800, by = 20), col = "lightgrey", border = "white",
     xlab = "Công suất tối đa (W)", ylab = "Tần số", main = "Notebook = No")
hist(GPU_power$Max_Power[GPU_power$Notebook_GPU == "Yes"],
     breaks = seq(0, 800, by = 20), col = "steelblue", border = "white",
     xlab = "Công suất tối đa (W)", ylab = "Tần số", main = "Notebook = Yes")
par(mfrow = c(1, 1))
ket_thuc()

bat_dau("hinh_boxplot_hang_notebook", 1300, 650)
luoi(1, 2)
boxplot(Max_Power ~ Manufacturer, data = GPU_power,
        col = mau_hang[sort(unique(GPU_power$Manufacturer))],
        xlab = "Hãng", ylab = "Công suất tối đa (W)")
boxplot(Max_Power ~ Notebook_GPU, data = GPU_power,
        col = mau_note[sort(unique(GPU_power$Notebook_GPU))],
        xlab = "Notebook GPU", ylab = "Công suất tối đa (W)")
par(mfrow = c(1, 1))
ket_thuc()

nhom <- interaction(GPU_power$Manufacturer, GPU_power$Notebook_GPU, drop = TRUE)
bat_dau("hinh_boxplot_4_nhom", 1300, 700)
luoi(1, 1, mar = c(9.5, 5.8, 1.5, 1))
boxplot(GPU_power$Max_Power ~ nhom, las = 2,
        col = ifelse(grepl("\\.Yes$", levels(nhom)), "steelblue", "lightgrey"),
        xlab = "", ylab = "Công suất tối đa (W)")
mtext("Hãng và nhãn notebook", side = 1, line = 7.8, cex = CEX_NHAN)
legend("topright", fill = c("lightgrey", "steelblue"),
       legend = c("Notebook = No", "Notebook = Yes"), bty = "n", cex = 1.4)
ket_thuc()

bat_dau("hinh_scatter", 1500, 900)
luoi(2, 3)
for (v in predictor_num) {
  x <- GPU_pre[[v]]
  y <- GPU_pre$Max_Power
  ok <- is.finite(x) & is.finite(y)
  plot(x[ok], y[ok], pch = 19, cex = 0.5, col = adjustcolor("steelblue", 0.4),
       xlab = nhan[[v]], ylab = nhan[["Max_Power"]])
  lines(lowess(x[ok], y[ok]), col = "red", lwd = 3)
}
par(mfrow = c(1, 1))
ket_thuc()

bat_dau("hinh_scatter_log", 1500, 900)
luoi(2, 3)
for (v in predictor_num) {
  x <- log(GPU_pre[[v]])
  y <- log(GPU_pre$Max_Power)
  ok <- is.finite(x) & is.finite(y)
  plot(x[ok], y[ok], pch = 19, cex = 0.5, col = adjustcolor("steelblue", 0.4),
       xlab = paste("ln", nhan[[v]]), ylab = "ln(công suất tối đa, W)")
  lines(lowess(x[ok], y[ok]), col = "red", lwd = 3)
}
par(mfrow = c(1, 1))
ket_thuc()

log_df <- data.frame(
  ln_Max_Power        = log(GPU_pre$Max_Power),
  ln_Core_Speed       = log(GPU_pre$Core_Speed),
  ln_Memory_Bandwidth = log(GPU_pre$Memory_Bandwidth),
  ln_Memory           = log(GPU_pre$Memory),
  ln_Process          = log(GPU_pre$Process)
)
log_df <- log_df[is.finite(rowSums(log_df)), ]
nrow(log_df)
bat_dau("hinh_ggpairs_log", 1200, 1200)
print(
  ggpairs(log_df, upper = list(continuous = wrap("cor", size = 5))) +
    theme(axis.text  = element_text(size = 11),
          strip.text = element_text(size = 13))
)
ket_thuc()

bat_dau("hinh_tuong_quan", 1200, 1000)
corrplot::corrplot(cor(GPU_power[num_vars]), method = "color", type = "upper",
                   addCoef.col = "black", tl.col = "black", tl.srt = 45,
                   tl.cex = 1.5, number.cex = 1.5, cl.cex = 1.3)
ket_thuc()
round(cor(GPU_power[num_vars]), 2)
round(cor(GPU_pre[num_vars], use = "pairwise.complete.obs"), 2)

power_list <- data.frame(
  Row_ID       = GPU_identity$.row_id,
  Name         = GPU_identity$Name,
  Manufacturer = GPU_power$Manufacturer,
  Notebook_GPU = GPU_power$Notebook_GPU,
  Max_Power    = GPU_power$Max_Power
)
power_list %>% arrange(Max_Power) %>% head(10)
power_list %>% arrange(desc(Max_Power)) %>% head(10)
sum(power_list$Max_Power %in% boxplot.stats(power_list$Max_Power)$out)

data.frame(Name = GPU_identity$Name, Memory_Bandwidth = GPU_power$Memory_Bandwidth) %>%
  arrange(desc(Memory_Bandwidth)) %>%
  head(5)
