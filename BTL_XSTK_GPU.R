library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(patchwork)
library(showtext)
library(stringr)
showtext_auto()

na_tokens <- c("", "-", "N/A", "None")

data <- read.csv("All_GPUs.csv", stringsAsFactors = FALSE) %>%
  mutate(across(where(is.character), ~ {
    x <- str_trim(.x)
    x[x %in% na_tokens] <- NA
    x
  }))

print(head(data[, 1:4]), row.names = FALSE)
print(dim(data))

na_summary_df <- data.frame(
  Variable        = names(data),
  Missing_Count   = colSums(is.na(data)),
  Missing_Percent = round(colMeans(is.na(data)) * 100, 2),
  row.names       = NULL
)

hien_thi_na <- function(p, bien) {
  p <- p + theme(
    plot.title = element_text(size = 14),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 12)
  )
  p$layers[[2]] <- geom_text(aes(label = .data[[bien]]),
    hjust = -0.1, size = 3)
  if (interactive()) print(p)
}

plot1 <- ggplot(data = na_summary_df, aes(x = reorder(Variable, Missing_Count), y = Missing_Count)) +
  geom_bar(stat = "identity", fill = "sienna3") +
  geom_text(aes(label = Missing_Count), hjust = -0.1, size = 20, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Số lượng dữ liệu khuyết theo biến",
    x = "Biến",
    y = "Số lượng NA"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 50, face = "bold", margin = margin(b = 20), hjust = 0.5),
    axis.text.y = element_text(size = 40, face = "bold", color = "black"),
    axis.text.x = element_text(size = 40, color = "black"),
    axis.title = element_text(size = 50, face = "bold")
  )
hien_thi_na(plot1, "Missing_Count")
ggsave("na_count_plot_large.png", plot = plot1, width = 14, height = 16, dpi = 300)

plot_na <- ggplot(data = na_summary_df, aes(x = reorder(Variable, Missing_Percent), y = Missing_Percent)) +
  geom_bar(stat = "identity", fill = "turquoise3") +
  geom_text(aes(label = Missing_Percent), hjust = -0.1, size = 20, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Tỷ lệ dữ liệu khuyết theo biến",
    x = "Biến",
    y = "Tỷ lệ NA (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 50, face = "bold", margin = margin(b = 20), hjust = 0.5),
    axis.text.y = element_text(size = 40, face = "bold", color = "black"),
    axis.text.x = element_text(size = 40, color = "black"),
    axis.title = element_text(size = 50, face = "bold")
  )
hien_thi_na(plot_na, "Missing_Percent")
ggsave("na_percentage_plot_large.png", plot = plot_na, width = 14, height = 16, dpi = 300)

num_vars <- c("Max_Power", "Core_Speed", "Memory_Bandwidth", "Memory", "TMUs", "Process")
cat_vars <- c("Manufacturer", "Notebook_GPU")
predictor_num <- setdiff(num_vars, "Max_Power")

GPU_power <- data %>% select(all_of(c(num_vars, cat_vars)))
GPU_power_3_3 <- GPU_power
GPU_identity <- data %>%
  transmute(.row_id = row_number(), Name, Manufacturer, Integrated)

print(data.frame(Variable = names(GPU_power)), row.names = FALSE)
print(dim(GPU_power))

num <- function(x) as.numeric(str_extract(str_remove_all(x, ","), "\\d+\\.?\\d*"))

has <- function(x, pattern) str_detect(x, regex(pattern, ignore_case = TRUE)) %in% TRUE

na_before_parse <- colSums(is.na(GPU_power[num_vars]))

GPU_power <- GPU_power %>%
  mutate(
    Max_Power        = num(Max_Power),
    Core_Speed       = num(Core_Speed)       * if_else(has(Core_Speed, "GHz"), 1000, 1),
    Memory_Bandwidth = num(Memory_Bandwidth) / if_else(has(Memory_Bandwidth, "MB/sec"), 1024, 1),
    Memory           = num(Memory)           * if_else(has(Memory, "GB"), 1024, 1),
    TMUs             = num(TMUs),
    Process          = num(Process),

    Notebook_GPU = case_when(
      str_to_lower(Notebook_GPU) %in% c("yes", "1", "true")  ~ "Yes",
      str_to_lower(Notebook_GPU) %in% c("no", "0", "false")  ~ "No",
      TRUE ~ NA_character_
    )
  )

parse_check <- data.frame(
  NA_truoc = na_before_parse,
  NA_sau   = colSums(is.na(GPU_power[num_vars]))
)
parse_check$NA_moi <- parse_check$NA_sau - parse_check$NA_truoc
print(parse_check)
str(GPU_power)
GPU_power_3_4_1 <- GPU_power

n_truoc <- nrow(GPU_power)
keep <- !is.na(GPU_power$Max_Power)
GPU_power <- GPU_power[keep, , drop = FALSE]
GPU_identity <- GPU_identity[keep, , drop = FALSE]
GPU_power_3_4_2 <- GPU_power

print(c(
  Removed_Missing_Max_Power = n_truoc - nrow(GPU_power),
  Remaining_Rows = nrow(GPU_power)
))

external_source_urls <- c(
  paste0(
    "https://www.intel.com/content/dam/support/us/en/documents/processors/",
    "3rd_2nd_legacy_desktop_feb-2021-web.pdf"
  ),
  paste0(
    "https://la.nvidia.com/content/PDF/kepler/",
    "Tesla-K10-Board-Spec-06280-001-v04.pdf"
  ),
  paste0(
    "https://www.nvidia.com/content/quadro_oem/linecards/",
    "Quadro_LC_HP_8Nov10_HR.pdf"
  ),
  paste0(
    "https://forums.developer.nvidia.com/t/nvidia-titan-x/43978"
  ),
  paste0(
    "https://www.amd.com/en/support/downloads/drivers.html/graphics/",
    "radeon-600-500-400/radeon-rx-400-series/radeon-rx-460.html"
  )
)

external_values <- data.frame(
  Row_ID = c(
    927, 1570, 1571, 1573, 1574, 1575, 1576, 1577, 1579, 1581, 1582, 1597, 1604, 1605,
    1609, 1611, 1612, 1616, 1617, 1618, 1619, 1620, 1621, 1623, 1625, 1627, 1628, 1629,
    17, 564, 43, 1351, 1351, 1353
  ),
  Name = c(
    "HD i5 K655", "HD i3 560", "HD i3 540", "HD i3 2130", "HD i3 550", "HD i3 2125",
    "HD i3 530", "HD i3 2100T", "HD i3 2120", "HD i3 2100", "HD i3 2105", "HD i3 3220",
    "HD i5 670", "HD i5 680", "HD i5 661", "HD i5 655K", "HD i5 660", "HD i5 2500",
    "HD i5 3550", "HD i5 2400", "HD i5 2310", "HD i5 2320", "HD i5 3570k",
    "HD i5 2300", "HD i5 2405S", "HD i5 2400S", "HD i5 650", "HD i5 2500K",
    "Tesla K10", "Quadro FX 3800M", "Titan X Pascal 2016", "Radeon RX 460 2GB",
    "Radeon RX 460 2GB", "Radeon RX 460 4GB"
  ),
  Manufacturer = c(
    "Intel", "Intel", "Intel", "Intel", "Intel", "Intel", "Intel", "Intel", "Intel",
    "Intel", "Intel", "Intel", "Intel", "Intel", "Intel", "Intel", "Intel", "Intel",
    "Intel", "Intel", "Intel", "Intel", "Intel", "Intel", "Intel", "Intel", "Intel",
    "Intel", "Nvidia", "Nvidia", "Nvidia", "ATI", "ATI", "ATI"
  ),
  Variable = c(
    "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed",
    "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed",
    "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed",
    "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed",
    "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed", "Core_Speed", "Memory",
    "Memory", "Core_Speed", "Memory_Bandwidth", "Memory_Bandwidth"
  ),
  Value = c(
    733, 733, 733, 850, 733, 850, 733, 650, 850, 850, 850, 650, 733, 733, 900, 733,
    733, 850, 650, 850, 850, 850, 650, 850, 850, 850, 733, 850, 745, 1024, 12288, 1090,
    112, 112
  ),
  Unit = c(
    "MHz", "MHz", "MHz", "MHz", "MHz", "MHz", "MHz", "MHz", "MHz", "MHz", "MHz", "MHz",
    "MHz", "MHz", "MHz", "MHz", "MHz", "MHz", "MHz", "MHz", "MHz", "MHz", "MHz", "MHz",
    "MHz", "MHz", "MHz", "MHz", "MHz", "MB", "MB", "MHz", "GB/s", "GB/s"
  ),
  Source_URL = external_source_urls[c(
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 3, 4, 5, 5, 5
  )],
  stringsAsFactors = FALSE
)

GPU_power_before_external <- GPU_power
external_audit <- external_values[FALSE, , drop = FALSE]
data_bo_sung <- read.csv("All_GPUs.csv", stringsAsFactors = FALSE,
                         colClasses = "character", na.strings = character())

stopifnot(!anyDuplicated(paste(external_values$Row_ID,
                               external_values$Variable, sep = "|")))
stopifnot(all(external_values$Variable %in% predictor_num))
stopifnot(all(is.finite(external_values$Value) & external_values$Value > 0))

for (j in seq_len(nrow(external_values))) {
  row <- external_values$Row_ID[j]
  variable <- external_values$Variable[j]
  stopifnot(row >= 1, row <= nrow(data_bo_sung))
  stopifnot(trimws(data_bo_sung$Name[row]) == trimws(external_values$Name[j]))
  stopifnot(trimws(data_bo_sung$Manufacturer[row]) ==
              trimws(external_values$Manufacturer[j]))
  raw_value <- as.character(data_bo_sung[[variable]][row])
  if (is.na(raw_value) || trimws(raw_value) %in% na_tokens) {
    unit <- external_values$Unit[j]
    if (unit == "GB/s") unit <- "GB/sec"
    data_bo_sung[[variable]][row] <- paste(external_values$Value[j], unit)
  }
  i <- match(row, GPU_identity$.row_id)
  if (is.na(i)) next
  if (is.na(GPU_power[[variable]][i])) {
    GPU_power[[variable]][i] <- external_values$Value[j]
    external_audit <- rbind(external_audit, external_values[j, , drop = FALSE])
  }
}

external_summary <- data.frame(
  Variable = predictor_num,
  Missing_Before = colSums(is.na(GPU_power_before_external[predictor_num])),
  Added_External = colSums(is.na(GPU_power_before_external[predictor_num])) -
    colSums(is.na(GPU_power[predictor_num])),
  Missing_After = colSums(is.na(GPU_power[predictor_num])),
  row.names = NULL
)

print(c(
  Raw_Added_Cells = nrow(external_values),
  Raw_GPUs = length(unique(external_values$Row_ID)),
  Analysis_Added_Cells = nrow(external_audit),
  Analysis_GPUs = length(unique(external_audit$Row_ID))
))
print(external_summary)
write.csv(data_bo_sung, "All_GPUs_bo_sung.csv", row.names = FALSE, na = "")
write.csv(external_values, "Nguon_bo_sung_da_doi_chieu.csv", row.names = FALSE)
write.csv(external_audit, "GPU_external_audit.csv", row.names = FALSE)
write.csv(external_summary, "GPU_external_summary.csv", row.names = FALSE)
write.csv(GPU_power, "GPU_power_truoc_trung_vi.csv", row.names = FALSE, na = "")
GPU_imputation_flags <- cbind(GPU_identity,
                              as.data.frame(is.na(GPU_power[predictor_num])))
write.csv(GPU_imputation_flags, "GPU_imputation_flags.csv", row.names = FALSE)

GPU_power <- GPU_power %>%
  mutate(across(all_of(cat_vars), ~ coalesce(.x, "Unknown")))

print(colSums(is.na(GPU_power[num_vars])))

global_med <- GPU_power %>%
  summarise(across(all_of(predictor_num), ~ median(.x, na.rm = TRUE)))

GPU_power <- GPU_power %>%
  group_by(Manufacturer) %>%
  mutate(across(all_of(predictor_num), ~ coalesce(.x, median(.x, na.rm = TRUE)))) %>%
  ungroup() %>%
  mutate(across(all_of(predictor_num), ~ coalesce(.x, global_med[[cur_column()]])))

print(dim(GPU_power))
print(data.frame(
  Variable = names(GPU_power),
  Class = vapply(GPU_power, function(x) class(x)[1], character(1))
), row.names = FALSE)

print(summary(GPU_power[num_vars[1:3]]))
print(summary(GPU_power[num_vars[4:6]]))
print(table(GPU_power$Manufacturer))
print(table(GPU_power$Notebook_GPU))

power_review <- data.frame(
  Row_ID = GPU_identity$.row_id,
  Name = GPU_identity$Name,
  Max_Power = GPU_power$Max_Power
) %>%
  filter(Max_Power <= 1)

print(power_review, row.names = FALSE)
write.csv(power_review, "GPU_power_can_doi_chieu.csv", row.names = FALSE)

print(dim(GPU_power))
print(data.frame(
  Variable = names(GPU_power),
  Missing_Count = colSums(is.na(GPU_power)),
  row.names = NULL
), row.names = FALSE)

write.csv(GPU_power, "GPU_power.csv", row.names = FALSE, na = "")

library(GGally)


GPU_pre <- read.csv("GPU_power_truoc_trung_vi.csv", na.strings = c("", "NA"),
                    stringsAsFactors = FALSE)

print(nrow(GPU_pre) == nrow(GPU_power))
print(nrow(GPU_identity) == nrow(GPU_power))

print(external_summary)

imputed <- data.frame(
  Variable  = predictor_num,
  n_imputed = colSums(GPU_imputation_flags[predictor_num]),
  pct       = round(colMeans(GPU_imputation_flags[predictor_num]) * 100, 2),
  row.names = NULL
)
print(imputed)

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
print(desc_main)
print(desc_pre)

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
print(group_desc(GPU_power, "Manufacturer"))
print(group_desc(GPU_power, "Notebook_GPU"))
print(group_desc(GPU_power, c("Manufacturer", "Notebook_GPU")))

print(table(GPU_power$Manufacturer, GPU_power$Notebook_GPU))
print(round(prop.table(table(GPU_power$Manufacturer)) * 100, 2))
print(round(prop.table(table(GPU_power$Notebook_GPU)) * 100, 2))

print(summary(GPU_power %>% select(all_of(cat_vars)) %>% mutate(across(everything(), factor))))

GPU_scope <- GPU_power %>%
  mutate(Name = GPU_identity$Name, Integrated = GPU_identity$Integrated) %>%
  filter(Manufacturer %in% c("AMD", "Nvidia"), Integrated == "No")
print(nrow(GPU_scope))
print(table(GPU_scope$Manufacturer, GPU_scope$Notebook_GPU))
print(group_desc(GPU_scope, c("Manufacturer", "Notebook_GPU")))

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
print(nrow(log_df))
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
print(round(cor(GPU_power[num_vars]), 2))
print(round(cor(GPU_pre[num_vars], use = "pairwise.complete.obs"), 2))

power_list <- data.frame(
  Row_ID       = GPU_identity$.row_id,
  Name         = GPU_identity$Name,
  Manufacturer = GPU_power$Manufacturer,
  Notebook_GPU = GPU_power$Notebook_GPU,
  Max_Power    = GPU_power$Max_Power
)
print(power_list %>% arrange(Max_Power) %>% head(10))
print(power_list %>% arrange(desc(Max_Power)) %>% head(10))
print(sum(power_list$Max_Power %in% boxplot.stats(power_list$Max_Power)$out))

print(
data.frame(Name = GPU_identity$Name, Memory_Bandwidth = GPU_power$Memory_Bandwidth) %>%
  arrange(desc(Memory_Bandwidth)) %>%
  head(5)
)

print(c(Duplicate_8_Variable_Rows = sum(duplicated(GPU_power))))

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

input_file <- "GPU_power_truoc_trung_vi.csv"

predictor_num <- c("Core_Speed", "Memory_Bandwidth", "Memory", "TMUs", "Process")
reg_formula <- Max_Power ~ Core_Speed + Memory_Bandwidth + Memory +
  TMUs + Process + Manufacturer + Notebook_GPU
prediction_data <- read.csv(input_file, stringsAsFactors = FALSE)
prediction_data <- prediction_data[, all.vars(reg_formula), drop = FALSE]
prediction_data$Manufacturer <- factor(
  prediction_data$Manufacturer, levels = c("AMD", "ATI", "Intel", "Nvidia")
)
prediction_data$Notebook_GPU <- factor(prediction_data$Notebook_GPU, levels = c("No", "Yes"))

set.seed(99, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
train_index <- sample.int(nrow(prediction_data), size = floor(0.7 * nrow(prediction_data)))
train_raw <- prediction_data[train_index, , drop = FALSE]
test_raw <- prediction_data[-train_index, , drop = FALSE]

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

print(c(N_All = nrow(prediction_data),
        N_Train = nrow(train_data), N_Test = nrow(test_data)))
print(round(prediction_results, 4))

rmse_lm <- prediction_results["Linear_Regression", "RMSE"]
rmse_rf <- prediction_results["Random_Forest", "RMSE"]
print(data.frame(
  RMSE_Reduction_W = rmse_lm - rmse_rf,
  RMSE_Reduction_Percent = 100 * (rmse_lm - rmse_rf) / rmse_lm
), row.names = FALSE)

comparison_data <- data.frame(
  Metric = rep(c("MAE", "RMSE"), each = 2),
  Model = rep(c("Hoi quy tuyen tinh boi", "Random Forest"), 2),
  Error = c(prediction_results[, "MAE"], prediction_results[, "RMSE"])
)
comparison_data$Metric <- factor(comparison_data$Metric, levels = c("MAE", "RMSE"))
comparison_data$Model <- factor(comparison_data$Model,
  levels = c("Hoi quy tuyen tinh boi", "Random Forest"))

comparison_plot <- ggplot(comparison_data, aes(x = Metric, y = Error, fill = Model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Error)),
    position = position_dodge(width = 0.8), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("steelblue", "darkorange"),
    labels = c("Hồi quy tuyến tính bội", "Random Forest")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Sai số (W)", fill = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right", panel.grid.major.x = element_blank())

print(comparison_plot)
ggsave("hinh_rf_mae_rmse.png", plot = comparison_plot,
  width = 10, height = 5.5, dpi = 150)
ggsave("hinh_rf_mae_rmse.pdf", plot = comparison_plot,
  width = 10, height = 5.5)
