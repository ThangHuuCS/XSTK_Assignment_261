library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(patchwork)
library(showtext)
library(stringr)
showtext_auto()

# =====================================================================
# BƯỚC 1: ĐỌC DỮ LIỆU VÀ NHẬN DIỆN KÝ HIỆU THIẾU (NA)
# Chuỗi rỗng, khoảng trắng, "\n-", "\n- ", "-" ... đều được đưa về NA.
# str_trim() cắt cả khoảng trắng lẫn "\n" ở hai đầu, nên "\n- " -> "-" -> NA.
# =====================================================================
na_tokens <- c("", "-", "N/A", "None")

data <- read.csv("All_GPUs.csv", stringsAsFactors = FALSE) %>%
  mutate(across(where(is.character), ~ {
    x <- str_trim(.x)
    x[x %in% na_tokens] <- NA
    x
  }))

head(data)

# =====================================================================
# BƯỚC 2: ĐỊNH LƯỢNG MỨC ĐỘ THIẾU
# =====================================================================
na_summary_df <- data.frame(
  Variable        = names(data),
  Missing_Count   = colSums(is.na(data)),
  Missing_Percent = round(colMeans(is.na(data)) * 100, 2),
  row.names       = NULL
)

# Ảnh 1: số lượng NA theo biến
plot1 <- ggplot(data = na_summary_df, aes(x = reorder(Variable, Missing_Count), y = Missing_Count)) +
  geom_bar(stat = "identity", fill = "sienna3") +
  geom_text(aes(label = Missing_Count), hjust = -0.1, size = 20, fontface = "bold") +
  coord_flip() + # lật ngang biểu đồ
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
ggsave("na_count_plot_large.png", plot = plot1, width = 14, height = 16, dpi = 300)

# Ảnh 2: tỷ lệ NA theo biến
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
ggsave("na_percentage_plot_large.png", plot = plot2, width = 14, height = 16, dpi = 300)

# =====================================================================
# BƯỚC 3: CHỌN 8 BIẾN CỦA GPU_power (các biến còn lại không xử lý)
# =====================================================================
num_vars  <- c("Max_Power", "Core_Speed", "Memory_Bandwidth", "Memory", "TMUs", "Process")
cat_vars  <- c("Manufacturer", "Notebook_GPU")

GPU_power <- data %>% select(all_of(c(num_vars, cat_vars)))

# Kiểm tra đơn vị đang có trong dữ liệu gốc 
for (v in c("Max_Power", "Core_Speed", "Memory_Bandwidth", "Memory", "Process")) {
  cat("\nĐơn vị xuất hiện trong", v, ":\n")
  print(table(str_extract(GPU_power[[v]], "[A-Za-z/]+")))
}

# =====================================================================
# BƯỚC 4: CHUẨN HÓA ĐƠN VỊ (làm TRƯỚC khi điền NA)
#   Max_Power        -> Watt
#   Core_Speed       -> MHz   (GHz x 1000)
#   Memory_Bandwidth -> GB/s  (MB/s / 1024)
#   Memory           -> MB    (GB x 1024)
#   Process          -> nm
#   TMUs             -> số đơn vị (không có đơn vị)
# =====================================================================
# Rút trích phần số (bỏ dấu phẩy ngăn cách hàng nghìn, vd "1,024")
num <- function(x) as.numeric(str_extract(str_remove_all(x, ","), "\\d+\\.?\\d*"))
# Kiểm tra chuỗi có chứa đơn vị cho trước không (không phân biệt hoa/thường; NA -> FALSE)
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

    # Notebook_GPU: nhị phân Yes/No. Giá trị lạ -> NA (sẽ thành "Unknown" ở bước 6)
    Notebook_GPU = case_when(
      str_to_lower(Notebook_GPU) %in% c("yes", "1", "true")  ~ "Yes",
      str_to_lower(Notebook_GPU) %in% c("no", "0", "false")  ~ "No",
      TRUE ~ NA_character_
    )
  )

# Kiểm tra: phép chuyển đổi không được tạo thêm NA mới (cột NA_moi phải bằng 0)
parse_check <- data.frame(
  NA_truoc = na_before_parse,
  NA_sau   = colSums(is.na(GPU_power[num_vars]))
)
parse_check$NA_moi <- parse_check$NA_sau - parse_check$NA_truoc
print(parse_check)

# =====================================================================
# BƯỚC 5: LOẠI CÁC DÒNG THIẾU BIẾN MỤC TIÊU (Max_Power)
# =====================================================================
n_truoc <- nrow(GPU_power)
GPU_power <- GPU_power %>% filter(!is.na(Max_Power))
cat("Số dòng bị loại do thiếu Max_Power:", n_truoc - nrow(GPU_power),
    "| Số dòng còn lại:", nrow(GPU_power), "\n")

# =====================================================================
# BƯỚC 6: ĐIỀN NA CÒN LẠI
#   - Biến phân loại (Manufacturer, Notebook_GPU): NA -> "Unknown"
#   - Biến định lượng: trung vị theo Manufacturer;
#     nếu trung vị của nhóm không xác định thì dùng trung vị toàn bộ
#     (trung vị toàn bộ tính từ dữ liệu quan sát được, TRƯỚC khi điền)
# =====================================================================
GPU_power <- GPU_power %>%
  mutate(across(all_of(cat_vars), ~ coalesce(.x, "Unknown")))

cat("\nSố NA định lượng cần điền trung vị:\n")
print(colSums(is.na(GPU_power[num_vars])))

global_med <- GPU_power %>%
  summarise(across(all_of(num_vars), ~ median(.x, na.rm = TRUE)))

GPU_power <- GPU_power %>%
  group_by(Manufacturer) %>%
  mutate(across(all_of(num_vars), ~ coalesce(.x, median(.x, na.rm = TRUE)))) %>%
  ungroup() %>%
  mutate(across(all_of(num_vars), ~ coalesce(.x, global_med[[cur_column()]])))

# =====================================================================
# BƯỚC 7: KIỂM TRA SAU XỬ LÝ
# =====================================================================
glimpse(GPU_power)

print("Tóm tắt thống kê các thông số tập GPU_power:")
print(summary(GPU_power))

print("Phân bố Manufacturer và Notebook_GPU:")
print(table(GPU_power$Manufacturer))
print(table(GPU_power$Notebook_GPU))

print("Kích thước dữ liệu sau khi làm sạch (Số dòng, Số cột):")
print(dim(GPU_power))

print("Số lượng NA còn lại ở từng cột:")
print(colSums(is.na(GPU_power)))