# Xác suất và thống kê - GPU dataset

## Thông tin chung
- **Project**: Phân tích Dữ liệu GPU (GPU Data Analysis)
- **Môn học**: Xác suất và Thống kê
- **Nhóm**: Nhóm 11

## Mô tả Dự án
Dự án thực hiện phân tích bộ dữ liệu về các bộ xử lý đồ họa (GPU) được lưu trữ trong tệp `All_GPUs.csv`. Mục tiêu là làm sạch, xử lý, trực quan hóa và diễn giải dữ liệu bằng các phương pháp thống kê.

## Tổng quan Báo cáo (`Project.R`)
Tệp R `Project.R` chứa toàn bộ mã nguồn và phần giải thích chi tiết cho từng bước phân tích. Các phần chính bao gồm:

### 1. Tiền xử lý Dữ liệu
- Nhập tệp CSV và xử lý các giá trị bị khuyết (missing values).
- Chuyển đổi các biến sang kiểu dữ liệu phù hợp (`numeric`, `character`, v.v.).
- Loại bỏ các cột có tỷ lệ dữ liệu bị khuyết cao.

### 2. Thống kê Mô tả
- Tính toán các chỉ số thống kê như trung bình (mean), trung vị (median), giá trị nhỏ nhất (min), giá trị lớn nhất (max), v.v.
- Trực quan hóa phân phối dữ liệu bằng biểu đồ tần suất (histogram) và biểu đồ hộp (boxplot).

### 3. Nhận xét và Kết luận
- So sánh mức giá trung bình giữa GPU của NVIDIA và AMD.
- Làm nổi bật các đặc trưng chính có ảnh hưởng đến giá của GPU.

## Thống kê mô tả (Phần 4)

[`MoTa.R`](MoTa.R) thực hiện thống kê mô tả theo phần 4 của báo cáo. File chỉ chứa lệnh R, không có comment hoặc lời giải thích.

- Script tự gọi `source("NewProject.R")` ở đầu file, nên khi chạy `MoTa.R` không cần chạy `NewProject.R` trước. Các đối tượng như `GPU_power`, `GPU_identity`, `GPU_imputation_flags`, `external_summary` được lấy từ lần chạy đó.
- Đối chiếu ba bảng `GPU_pre` (đọc từ `GPU_power_truoc_trung_vi.csv`), `GPU_power` và `GPU_identity` có cùng số dòng (2781), in `external_summary` và tỷ lệ ô phải điền trung vị của từng biến giải thích.
- Bảng thống kê mẫu cho 6 biến định lượng (Mean, SD, Min, Q1, Median, Q3, Max) trước khi điền trung vị (`desc_pre`, kèm cỡ mẫu n) và sau khi điền trung vị (`desc_main`).
- Thống kê `Max_Power` theo `Manufacturer`, theo `Notebook_GPU` và theo tổ hợp hai biến; bảng tần số, tỷ lệ phần trăm, bảng chéo hãng × notebook. `GPU_scope` giới hạn ở AMD và Nvidia không tích hợp để đối chiếu xu hướng.
- Đồ thị được lưu thành file PNG (150 dpi) trong thư mục chạy: `hinh_barplot`, `hinh_hist_max_power` (thang gốc và thang log), `hinh_hist_cac_bien`, `hinh_hist_theo_notebook`, `hinh_boxplot_hang_notebook`, `hinh_boxplot_4_nhom`, `hinh_scatter`, `hinh_scatter_log` (kèm đường lowess), `hinh_ggpairs_log` và `hinh_tuong_quan`. Đặt `luu_hinh <- FALSE` để không lưu file.
- Ma trận tương quan Pearson được in cho `GPU_power` (sau điền trung vị) và `GPU_pre` (trước điền trung vị, dùng các cặp đầy đủ). Script cũng liệt kê 10 giá trị `Max_Power` nhỏ nhất và lớn nhất, đếm ngoại lai theo `boxplot.stats` và liệt kê 5 GPU có `Memory_Bandwidth` cao nhất.

Cài thêm `GGally` một lần trong R:

```r
install.packages("GGally")
```

Chạy trong thư mục repo:

```sh
Rscript MoTa.R
```

Với dữ liệu hiện tại, tập phân tích có 2781 quan sát; `Max_Power` có trung bình khoảng 125,60 W, độ lệch chuẩn khoảng 102,89 W, trung vị 105 W, và có 94 quan sát được `boxplot.stats` xem là ngoại lai.

## Thống kê suy diễn (Phần 5)

[`ThongKeSuyDien_Phan5.R`](ThongKeSuyDien_Phan5.R) thực hiện ANOVA hai nhân tố, so sánh bội Tukey HSD và hồi quy tuyến tính bội theo phần 5 của báo cáo. File chỉ chứa lệnh R, không có comment hoặc lời giải thích.

- ANOVA dùng `Manufacturer * Notebook_GPU`, loại ATI riêng vì không có tổ hợp ATI–Yes. Hồi quy vẫn giữ đủ bốn nhãn `Manufacturer`.
- Hồi quy chia train/test 70/30 với seed `99`. Trung vị dùng điền khuyết chỉ được tính từ train và áp dụng cho cả hai tập.
- Script in các kiểm định, hệ số, khoảng tin cậy, VIF/GVIF và MAE, MSE, RMSE, R² trên test. Các dự báo âm được giữ khi tính sai số.

Cài thêm `car` và `lmtest` một lần trong R:

```r
install.packages(c("car", "lmtest"))
```

Chạy trong thư mục repo:

```sh
Rscript NewProject.R
Rscript ThongKeSuyDien_Phan5.R
```

`NewProject.R` tạo `GPU_power.csv` và `GPU_power_truoc_trung_vi.csv` làm đầu vào. Với dữ liệu hiện tại, ANOVA có 2689 quan sát, hồi quy có 1946 dòng train và 835 dòng test; MAE khoảng 27,19 W và RMSE khoảng 42,36 W.

## So sánh mô hình dự báo `Max_Power` (Phần 6)

[`MoHinh_Phan6.R`](MoHinh_Phan6.R) so sánh hồi quy tuyến tính bội và Random Forest để dự báo công suất tối đa của GPU (W). Hai mô hình dùng cùng bảy biến giải thích và cùng phép chia 70/30 với seed `99`. Trung vị dùng để điền khuyết được tính từ tập huấn luyện.

### Cách chạy

Cài thêm gói `randomForest` trong R.

```r
install.packages("randomForest")
```

Chạy hai lệnh sau trong thư mục repo. `NewProject.R` đọc `All_GPUs.csv` và tạo `GPU_power_truoc_trung_vi.csv` làm đầu vào cho bước so sánh.

```sh
Rscript NewProject.R
Rscript MoHinh_Phan6.R
```

Script in MAE, MSE, RMSE, R² và số dự báo âm trên tập kiểm tra. Với 835 quan sát kiểm tra của lần chia này, Random Forest giảm RMSE từ **42,36 W** xuống **28,67 W** (giảm **32,32%**) so với hồi quy tuyến tính bội.

## Yêu cầu Hệ thống
- Phiên bản R >= 4.0
- Các gói thư viện (packages) cần thiết: `dplyr`, `ggplot2`, `lubridate`, `corrplot`, `patchwork`, `showtext`, `stringr`; phần 4 dùng thêm `GGally`; phần 5 dùng thêm `car`, `lmtest`; phần 6 dùng thêm `randomForest`

## Tác giả
- Võ Hoàng Phi - 2510188
- Nguyễn Ngọc Nguyên Tài - 2510220
- Nguyễn Hữu Thắng - 2510225
- Chế Quang Minh Toàn - 2510244
- Khúc Thế Hồng Phong - 2510355
