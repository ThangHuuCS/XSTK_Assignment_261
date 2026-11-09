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
- Các gói thư viện (packages) cần thiết: `dplyr`, `ggplot2`, `lubridate`, `corrplot`, `patchwork`, `showtext`, `stringr`

## Tác giả
- Võ Hoàng Phi - 2510188
- Nguyễn Ngọc Nguyên Tài - 2510220
- Nguyễn Hữu Thắng - 2510225
- Chế Quang Minh Toàn - 2510244
- Khúc Thế Hồng Phong - 2510355