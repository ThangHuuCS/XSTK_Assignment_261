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
print(plot1)
ggsave("na_count_plot_large.png", plot = plot1, width = 14, height = 16, dpi = 300)

plot2 <- ggplot(data = na_summary_df, aes(x = reorder(Variable, Missing_Percent), y = Missing_Percent)) +
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
print(plot2)
ggsave("na_percentage_plot_large.png", plot = plot2, width = 14, height = 16, dpi = 300)

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
