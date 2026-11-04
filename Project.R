library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(patchwork)
library(showtext)
library(stringr)
showtext_auto()

data <- read.csv(
    "All_GPUs.csv", 
    na.strings = c("", "\n", "N/A", "None", "\n- ", "\n-")
)

head(data)

na_summary_df <- data.frame(
  Variable = names(data),
  Missing = colSums(is.na(data)),
  Percentage = round(colMeans(is.na(data)) * 100, 1)
)

na_summary_df$Variable <- factor(na_summary_df$Variable, 
                                 levels = na_summary_df$Variable[order(na_summary_df$Missing)])

# Ảnh 1
plot1 <- ggplot(data = na_summary_df, aes(x = reorder(Variable, Missing), y = Missing)) +
  geom_bar(stat = "identity", fill = "sienna3") +
  # Chuyển vjust thành hjust vì biểu đồ đã lật ngang
  geom_text(aes(label = Missing), hjust = -0.1, size = 20, fontface = "bold") + 
  coord_flip() + # LẬT NGANG BIỂU ĐỒ
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

  # 3. Tạo biểu đồ 2: TỶ LỆ (Cũng lật ngang)
plot2 <- ggplot(data = na_summary_df, aes(x = reorder(Variable, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "turquoise3") +
  geom_text(aes(label = Percentage), hjust = -0.1, size = 20, fontface = "bold") +
  coord_flip() + # LẬT NGANG BIỂU ĐỒ
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Tỷ lệ dữ liệu khuyết theo biến",
    x = "Biến", 
    y = "Tỷ lệ NA (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 50, face = "bold", margin = margin(b = 20), hjust = 0.5),
    # Khôi phục trục Y (tên biến) do biểu đồ đã đứng độc lập
    axis.text.y = element_text(size = 40, face = "bold", color = "black"), 
    axis.text.x = element_text(size = 40, color = "black"),
    axis.title = element_text(size = 50, face = "bold") 
  )

# 5. Lưu ảnh với kích thước lớn (width = 16, height = 10) và độ nét cao (dpi = 300)
ggsave("na_percentage_plot_large.png", plot = plot2, width = 14, height = 16, dpi = 300)

# ---------------- CHỌN LỌC VÀ XỬ LÍ DỮ LIỆU KHUYẾT --------------------
# Hàm tìm Mode cho dữ liệu chữ
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data_selected <- data %>%
  select(Name, Manufacturer, Notebook_GPU, Architecture, Process,
         Core_Speed, Shader, TMUs, ROPs, Pixel_Rate, Texture_Rate,
         Memory, Memory_Speed, Memory_Bus, Memory_Bandwidth, 
         Max_Power, Release_Date)

# 2. Làm sạch dữ liệu (Sử dụng regex để tự động rút trích con số)
data_clean <- data_selected %>%
  mutate(
    # Thay vì gsub từng chữ, str_extract rút lấy phần số (bao gồm cả số thập phân)
    Core_Speed   = as.numeric(str_extract(Core_Speed, "\\d+\\.?\\d*")),
    Memory       = as.numeric(str_extract(Memory, "\\d+\\.?\\d*")),
    Memory_Speed = as.numeric(str_extract(Memory_Speed, "\\d+\\.?\\d*")),
    Memory_Bus   = as.numeric(str_extract(Memory_Bus, "\\d+\\.?\\d*")),
    Max_Power    = as.numeric(str_extract(Max_Power, "\\d+\\.?\\d*")),
    Pixel_Rate   = as.numeric(str_extract(Pixel_Rate, "\\d+\\.?\\d*")),
    Texture_Rate = as.numeric(str_extract(Texture_Rate, "\\d+\\.?\\d*")),
    Process      = as.numeric(str_extract(Process, "\\d+\\.?\\d*")),
    ROPs         = as.numeric(str_extract(ROPs, "\\d+\\.?\\d*")),
    TMUs         = as.numeric(TMUs),
    Shader       = as.numeric(Shader),
    
    # Logic chuẩn hóa Băng thông (Bỏ vòng lặp For)
    Memory_Bandwidth = case_when(
      grepl("MB/sec", Memory_Bandwidth) ~ as.numeric(str_extract(Memory_Bandwidth, "\\d+\\.?\\d*")) / 1000,
      grepl("GB/sec", Memory_Bandwidth) ~ as.numeric(str_extract(Memory_Bandwidth, "\\d+\\.?\\d*")),
      TRUE ~ as.numeric(str_extract(Memory_Bandwidth, "\\d+\\.?\\d*"))
    ),
    
    # Chuyển đổi Notebook_GPU thành dạng nhị phân (1/0)
    Notebook_GPU = ifelse(Notebook_GPU == "Yes", 1, 0)
  )

# 3. Lọc bỏ các dòng không có biến mục tiêu (Max_Power)
data_clean <- data_clean %>% filter(!is.na(Max_Power))
data_clean <- data_clean %>%
  group_by(Manufacturer) %>%
  mutate(
    across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))
  ) %>%
  ungroup() %>%
  mutate(
    # Bổ sung trung vị toàn bộ nếu có nhóm hãng bị khuyết 100% một biến nào đó
    across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)),
    # Điền "Unknown" cho các biến phân loại dạng chữ
    across(where(is.character), ~ifelse(is.na(.), "Unknown", .))
  )
glimpse(data_clean)
print("Tóm tắt thống kê các thông số:")
print(summary(data_clean))
print("Kích thước dữ liệu sau khi làm sạch:")
print(dim(data_clean))
print("Số lượng NA còn lại ở từng cột:")
print(colSums(is.na(data_clean)))