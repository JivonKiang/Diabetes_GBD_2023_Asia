rm(list = ls())
library(tools)

####----------------------------------------------------------------------------
###                有一个内嵌文件夹external_publications是需要载入的，要复制过来
####----------------------------------------------------------------------------

# 设置工作目录
target_dir <- "E:/20250414 Wenping Gong gaint mission/20260318 Yufeng Li/GBD数据下载 20260324"
setwd(target_dir)  

###----location位置排排坐
library(dplyr)
library(tidyr)
library(openxlsx)
library(purrr)

# 加载地理位置层次数据
locations <- read.csv("external_publications/hierarchies/location_GBD2021.csv") %>% 
  select(location_id, parent_id, location_name, sort_order, region_name, location_type) %>% 
  mutate(parent_id = ifelse(parent_id == 1, NA, parent_id))

# 筛选地区（保留所有符合条件的地区，包含上级parent地区）
locations <- locations %>%
  filter(
    grepl("High-income Asia Pacific|East Asia", region_name, ignore.case = TRUE),
    !location_type %in% c("admin1", "admin2"),
    !region_name %in% c("Southeast Asia")
  )
# 提取固定地区清单（核心：强制保留所有地区，含parent）
loc_master <- locations %>% select(location_id, location_name, sort_order, parent_id)

# 读取数据
AS <- read.csv("1-Age-Standardized Rate (ASR) for four indicators/IHME-GBD_2023_DATA-eda9680c-1.csv")
TPC <- read.csv("2-TPC for four indicators/IHME-GBD_2023_DATA-25256390-1.csv")

TPC <- TPC[,-15]
TPC[,15] <- c("TPC")
colnames(TPC)[15] <- c("year")

stopifnot(all(colnames(AS) == colnames(TPC)))
data <- rbind(AS, TPC)

# ===================== 计算 MIR (死亡率/发病率比) =====================
mir_data <- data %>%
  filter(measure_name %in% c("Deaths", "Incidence")) %>%
  select(location_id, location_name, cause_name, year, measure_name, val, lower, upper) %>%
  pivot_wider(
    id_cols = c(location_id, location_name, cause_name, year),
    names_from = measure_name,
    values_from = c(val, lower, upper)
  ) %>%
  mutate(
    val_MIR = val_Deaths / val_Incidence,
    lower_MIR = lower_Deaths / upper_Incidence,
    upper_MIR = upper_Deaths / lower_Incidence
  ) %>%
  pivot_longer(
    cols = c(val_MIR, lower_MIR, upper_MIR),
    names_to = c(".value", "measure_name"),
    names_sep = "_"
  ) %>%
  left_join(
    data %>% select(-val, -lower, -upper, -measure_name) %>% distinct(),
    by = c("location_id", "location_name", "cause_name", "year")
  ) %>%
  mutate(measure_name = "MIR")

# 合并MIR + 强制保留所有筛选地区（含parent，绝不丢失）
data <- bind_rows(data, mir_data) %>%
  right_join(loc_master, by = c("location_id", "location_name"))
# ==================================================================

# 整理指标名称
data <- data %>%
  mutate(across(c(measure_name, cause_name), as.character)) %>% 
  mutate(measure_name = case_when(
    measure_name == "DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
    TRUE ~ measure_name
  ))

# 格式化数值（缺失/无限值显示为 -）
df_formatted <- data %>%
  mutate(
    value_str = sprintf("%.2f (%.2f to %.2f)", val, lower, upper),
    value_str = ifelse(is.na(val) | !is.finite(val), "-", value_str),
    header_key = paste(cause_name, measure_name, year, sep = "|")
  )

# ===================== 生成总表 =====================
df_wide <- df_formatted %>%
  arrange(cause_name, measure_name, year) %>% 
  pivot_wider(
    id_cols = location_name,
    names_from = header_key,
    values_from = value_str
  )
# 补全所有地区
df_wide <- loc_master %>% select(location_name) %>% left_join(df_wide, by = "location_name")

# 表头与Excel输出
headers <- df_formatted %>% distinct(cause_name, measure_name, year) %>% arrange(cause_name, measure_name, year)
header_matrix <- rbind(headers$cause_name, headers$measure_name, headers$year) %>% t()

wb <- createWorkbook()
addWorksheet(wb, "Results")
writeData(wb, 1, t(header_matrix), startRow = 1, startCol = 2, colNames = FALSE)
writeData(wb, 1, df_wide, startRow = 4, colNames = TRUE)

# 合并单元格
for(lvl in 1:3) {
  current_level <- na.omit(header_matrix[lvl, ])
  rle_result <- rle(current_level)
  end_cols <- cumsum(rle_result$lengths)
  start_cols <- c(1, end_cols[-length(end_cols)] + 1)
  for(i in seq_along(rle_result$lengths)) {
    if(rle_result$lengths[i] > 1) {
      mergeCells(wb, 1, cols = (start_cols[i]+1):(end_cols[i]+1), rows = lvl)
    }
  }
}

header_style <- createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "TopBottomLeftRight")
addStyle(wb, 1, header_style, rows = 1:3, cols = 1:ncol(header_matrix))
saveWorkbook(wb, "formatted_results.xlsx", overwrite = TRUE)

#### ===================== 分病种表（核心：parent地区数据完整展示） =====================
# 【修复1】剔除NA/空值的疾病名称，防止Sheet名报错
cause_list <- unique(df_formatted$cause_name)
cause_list <- cause_list[!is.na(cause_list) & cause_list != ""]

wb <- createWorkbook()

for(cause in cause_list) {
  df_cause <- df_formatted %>% 
    filter(cause_name == cause) %>% 
    arrange(measure_name, year)
  
  # 宽格式转换 【修复2】添加 values_fn = first 解决数据重复问题
  df_wide_cause <- df_cause %>%
    pivot_wider(
      id_cols = location_name,
      names_from = c(measure_name, year),
      values_from = value_str,
      values_fn = first  # 关键：处理重复数据，不生成列表列
    )
  
  # ====== 核心：保留所有地区+排序+展示parent真实数据 ======
  df_wide_cause <- loc_master %>%
    left_join(df_wide_cause, by = "location_name") %>%
    arrange(sort_order) %>%  # 按GBD官方排序，parent在前
    left_join(locations %>% select(parent_id = location_id, parent_name = location_name), by = "parent_id") %>%
    mutate(parent_name = case_when(
      location_name == "Global" ~ "Global",
      grepl("SDI", location_name) ~ "SDI region",
      TRUE ~ parent_name
    )) %>%
    # 分组名称只显示一次，保留数据行
    mutate(parent_name = ifelse(row_number() == 1 | parent_name != lag(parent_name), parent_name, NA_character_)) %>%
    select(parent_name, location_name, everything(), -location_id, -parent_id, -sort_order)
  
  # 最终数据（无删除行，parent地区完整展示数值）
  df_final <- df_wide_cause
  
  # 表头处理
  headers_cause <- df_cause %>% distinct(measure_name, year) %>% arrange(measure_name, year)
  header_matrix_cause <- rbind(rep(cause, nrow(headers_cause)), headers_cause$measure_name, headers_cause$year)
  
  # 【修复3】Sheet名强制处理：杜绝NA，合规命名
  sheet_name <- gsub("[/\\:*?\\[\\]]", "_", substr(cause, 1, 31))
  sheet_name <- ifelse(is.na(sheet_name) | sheet_name == "", "Unknown_Cause", sheet_name)
  
  addWorksheet(wb, sheetName = sheet_name)
  
  # 写入数据
  writeData(wb, sheet_name, header_matrix_cause, startRow = 1, startCol = 2, colNames = FALSE)
  writeData(wb, sheet_name, df_final, startRow = 4, colNames = TRUE)
  
  # 合并表头
  for(lvl in 1:3) {
    rle_result <- rle(header_matrix_cause[lvl, ])
    end_cols <- cumsum(rle_result$lengths)
    start_cols <- c(1, end_cols[-length(end_cols)] + 1)
    for(i in seq_along(rle_result$lengths)) {
      if(rle_result$lengths[i] > 1) {
        mergeCells(wb, sheet_name, cols = (start_cols[i]+1):(end_cols[i]+1), rows = lvl)
      }
    }
  }
  
  # 样式设置
  header_style <- createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "TopBottomLeftRight")
  addStyle(wb, sheet_name, header_style, rows = 1:3, cols = 1:ncol(header_matrix_cause), gridExpand = TRUE)
  setColWidths(wb, sheet_name, cols = 1:ncol(df_final), widths = c(25, 25, rep(20, ncol(df_final)-2)))
}

# 保存文件
saveWorkbook(wb, "formatted_by_cause.xlsx", overwrite = TRUE)
cat("\n✅ 文件已保存：所有地区（含parent）数据完整展示！")