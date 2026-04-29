# 初始化环境 ----------------------------------------------------------------
rm(list = ls())

# 加载必要包
library(tools)
library(openxlsx)
library(nih.joinpoint)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(tidyr)
library(forcats)
library(scales)
library(parallel)

# 设置工作目录
target_dir <- "E:/20250414 Wenping Gong gaint mission/20260318 Yufeng Li/GBD数据下载 20260324"  # 注意使用正斜杠或双反斜杠
setwd(target_dir)  

# 创建主输出目录
output_main_dir <- "Joinpoint_Analysis_Results"
dir.create(output_main_dir, showWarnings = FALSE, recursive = TRUE)

###----location位置排排坐
# 加载地理位置层次数据
locations <- read.csv("external_publications/hierarchies/location_GBD2021.csv") %>% 
  select(location_id, parent_id, location_name, sort_order, region_name, location_type) %>% 
  mutate(parent_id = ifelse(parent_id == 1, NA, parent_id))

# 自动获取ZIP文件并处理路径
data_all <- read.csv("3-Jointpoint regression analysis/IHME-GBD_2023_DATA-c0cc9e26-1.csv")

# 转换列类型并执行替换
data_all <- data_all %>%
  mutate(across(c(measure_name, cause_name), as.character)) %>% 
  mutate(measure_name = case_when(
    measure_name == "DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
    TRUE ~ measure_name
  ))

# 重命名列以便于使用
data_all <- data_all %>%
  rename(
    measure = measure_name,
    cause = cause_name,
    location = location_name
  )

# 获取所有cause
causes <- unique(data_all$cause)

###----location位置排排坐
# 加载地理位置层次数据
library(dplyr)
locations <- read.csv("external_publications/hierarchies/location_GBD2021.csv") %>% 
  select(location_id, parent_id, location_name, sort_order, region_name, location_type) %>% 
  mutate(parent_id = ifelse(parent_id == 1, NA, parent_id))  # 将Global的parent设为NA

### 新增筛选代码开始 ###
library(openxlsx)
locations <- locations %>%
  filter(
    grepl("High-income Asia Pacific|East Asia", region_name, ignore.case = TRUE),  # 匹配包含 "Asia" 的名称（忽略大小写）
    !location_type %in% c("admin1", "admin2"),          # 排除 admin1 和 admin2 类型
    !region_name %in% c("Southeast Asia")          # 排除 admin1 和 admin2 类型
  )

# --------------------------
# 关键步骤：提取筛选后的地区名称
# --------------------------
selected_locations <- unique(locations$location_name)

# --------------------------
# 用这些地区筛选 data_all
# --------------------------
data_all <- data_all %>%
  filter(location %in% selected_locations)


# 主循环处理每个cause ----------------------------
walk(causes, function(current_cause) {
  # 创建cause专用目录
  cause_safe_name <- gsub("[^[:alnum:]]", "_", current_cause)
  cause_dir <- file.path(output_main_dir, cause_safe_name)
  dir.create(cause_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 筛选并处理数据
  data_processed <- data_all %>%
    filter(cause == current_cause) %>%
    filter(val != 0) %>%
    arrange(desc(location))
  
  # 验证数据是否有效
  if (nrow(data_processed) == 0) {
    message(sprintf("跳过原因 [%s]: 无有效数据", current_cause))
    return()
  }
  
  # 保存处理后的数据
  write.xlsx(data_processed, file.path(cause_dir, "combined_data.xlsx"))
  
  # 设置Joinpoint输出目录
  output_dir <- file.path(cause_dir, "join_point")
  dir.create(file.path(output_dir, "plots"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "rds"), recursive = TRUE, showWarnings = FALSE)
  
  # 核心分析函数 --------------------------------
  run_measure_analysis <- function(measure_type) {
    # 读取并预处理数据
    analysis_data <- tryCatch({
      df <- data_processed %>% 
        filter(measure == measure_type) %>%
        mutate(
          se = (upper - lower)/(2*qnorm(0.975)),
          year = as.numeric(year),
          value = val
        ) %>%
        group_by(location, year) %>%
        slice(1) %>%
        ungroup() %>%
        arrange(location, year) %>%
        filter(!is.na(value) & !is.na(year) & !is.na(se))
      
      # 验证每个location的数据量
      valid_locations <- df %>%
        group_by(location) %>%
        summarise(n = n(), .groups = "drop") %>%
        filter(n >= 6)  # Joinpoint至少需要6个数据点
      
      df %>% filter(location %in% valid_locations$location)
    }, error = function(e) {
      message(sprintf("数据加载失败 [%s-%s]: %s", current_cause, measure_type, e$message))
      return(NULL)
    })
    
    # 检查数据是否有效
    if (is.null(analysis_data) || nrow(analysis_data) == 0) {
      message(sprintf("跳过分析 [%s-%s]: 无有效数据", current_cause, measure_type))
      return(NULL)
    }
    
    # 执行Joinpoint分析
    jp_result <- tryCatch({
      joinpoint(
        data = analysis_data,
        x = "year",
        y = "value",
        by = "location",
        se = "se",
        run_opts = run_options(model = "ln", max_joinpoints = 5)
      )
    }, error = function(e) {
      message(sprintf("Joinpoint分析失败 [%s-%s]: %s", current_cause, measure_type, e$message))
      return(NULL)
    })
    
    if (is.null(jp_result)) return(NULL)
    
    # 保存原始RDS结果
    rds_file <- file.path(output_dir, "rds", 
                          paste0(gsub("[^[:alnum:]]", "_", measure_type), "_joinpoint.rds"))
    saveRDS(jp_result, rds_file)
    
    # 可视化输出
    if (length(unique(analysis_data$location)) > 0) {
      plot_name <- paste0(gsub("[^[:alnum:]]", "_", measure_type), "_trend.pdf")
      pdf(file.path(output_dir, "plots", plot_name), 
          width = 10, height = max(8, length(unique(analysis_data$location)) * 3))
      print(jp_plot(jp_result, ncol = 1))
      dev.off()
    }
    
    # 结果处理
    list(
      apc = jp_result$apc %>% mutate(measure = measure_type),
      report = jp_result$report %>% mutate(measure = measure_type),
      metadata = tibble(
        measure = measure_type,
        Analysis_Date = Sys.Date(),
        Data_Points = nrow(analysis_data),
        Joinpoints = ifelse(is.null(jp_result$report) || nrow(jp_result$report) == 0, 
                            NA, max(jp_result$report$segment, na.rm = TRUE))
      )
    )
  }
  
  # 执行分析
  measures <- unique(data_processed$measure)
  analysis_results <- map(measures, function(m) {
    result <- safely(run_measure_analysis)(m)
    if (!is.null(result$error)) {
      message(sprintf("分析错误 [%s-%s]: %s", current_cause, m, result$error$message))
    }
    result
  })
  
  # 保存结果到Excel
  wb <- createWorkbook()
  walk2(analysis_results, measures, ~{
    if (!is.null(.x$result)) {
      sheet_name_apc <- str_sub(paste0(.y, "_APC"), 1, 31)
      sheet_name_report <- str_sub(paste0(.y, "_Report"), 1, 31)
      
      if (!is.null(.x$result$apc) && nrow(.x$result$apc) > 0) {
        addWorksheet(wb, sheet_name_apc)
        writeDataTable(wb, sheet = sheet_name_apc, x = .x$result$apc, startRow = 2)
      }
      
      if (!is.null(.x$result$report) && nrow(.x$result$report) > 0) {
        addWorksheet(wb, sheet_name_report)
        writeDataTable(wb, sheet = sheet_name_report, x = .x$result$report, startRow = 2)
      }
    }
  })
  
  if (length(names(wb)) > 0) {
    saveWorkbook(wb, file.path(output_dir, "tables", "Joinpoint_Results.xlsx"), overwrite = TRUE)
  } else {
    message(sprintf("无有效结果可保存 [%s]", current_cause))
  }
  
  # 保存元数据
  metadata <- map_dfr(analysis_results, ~ if (!is.null(.x$result) && !is.null(.x$result$metadata)) .x$result$metadata else NULL)
  
  if (nrow(metadata) > 0) {
    write.xlsx(metadata, file.path(output_dir, "tables", "Analysis_Metadata.xlsx"))
  }
})

# 合并所有结果并进行可视化 ----------------------------------------

# 设置预设位置列表
# 预设位置列表（按照您提供的顺序）
preset_locations <- c(
  "Central Asia",
  "Armenia",
  "Azerbaijan", 
  "Georgia",
  "Kazakhstan",
  "Kyrgyzstan",
  "Mongolia",
  "Tajikistan",
  "Turkmenistan",
  "Uzbekistan",
  "Australasia",
  "Australia",
  "New Zealand",
  "High-income Asia Pacific",
  "Brunei Darussalam",
  "Japan",
  "Republic of Korea",
  "Singapore",
  "South Asia",
  "Bangladesh",
  "Bhutan",
  "India",
  "Nepal",
  "Pakistan",
  "East Asia",
  "China",
  "Democratic People's Republic of Korea",
  "Taiwan (Province of China)",
  "Southeast Asia",
  "Cambodia",
  "Indonesia",
  "Lao People's Democratic Republic",
  "Malaysia",
  "Maldives",
  "Mauritius",
  "Myanmar",
  "Philippines",
  "Seychelles",
  "Sri Lanka",
  "Thailand",
  "Timor-Leste",
  "Viet Nam"
)

# 创建结果汇总目录
summary_dir <- file.path(output_main_dir, "Summary_Results")
dir.create(summary_dir, showWarnings = FALSE, recursive = TRUE)

# 读取所有结果文件并合并
combined_apc_list <- list()

for (current_cause in causes) {
  cause_safe_name <- gsub("[^[:alnum:]]", "_", current_cause)
  result_file <- file.path(output_main_dir, cause_safe_name, "join_point/tables/Joinpoint_Results.xlsx")
  
  if (!file.exists(result_file)) next
  
  # 获取所有工作表名称
  sheets <- openxlsx::getSheetNames(result_file)
  apc_sheets <- sheets[str_detect(sheets, "_APC$")]
  
  if (length(apc_sheets) == 0) next
  
  # 读取所有APC工作表并合并
  for (sht in apc_sheets) {
    tryCatch({
      apc_data <- read.xlsx(result_file, sheet = sht) %>%
        mutate(
          cause = current_cause,
          measure = str_remove(sht, "_APC")
        )
      combined_apc_list[[paste(current_cause, sht)]] <- apc_data
    }, error = function(e) {
      message(sprintf("读取失败 [%s-%s]: %s", current_cause, sht, e$message))
    })
  }
}

# 合并所有APC数据
if (length(combined_apc_list) > 0) {
  combined_apc <- bind_rows(combined_apc_list) %>%
    mutate(
      segment_start = as.numeric(segment_start),
      segment_end = as.numeric(segment_end),
      time_interval = paste(segment_start, segment_end, sep = "-"),
      significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      ),
      trend = ifelse(apc > 0, "Increase", "Decrease"),
      location = as.character(location)
    ) %>%
    filter(!is.na(apc) & !is.na(location))
  
  # 保存合并后的APC数据
  write.xlsx(combined_apc, file.path(summary_dir, "All_Causes_APC.xlsx"))
  
  # 可视化每个cause
  walk(causes, function(current_cause) {
    cause_safe_name <- gsub("[^[:alnum:]]", "_", current_cause)
    cause_dir <- file.path(output_main_dir, cause_safe_name)
    vis_dir <- file.path(cause_dir, "vis")
    dir.create(vis_dir, showWarnings = FALSE, recursive = TRUE)
    
    # 筛选当前cause的数据
    cause_data <- combined_apc %>%
      filter(cause == current_cause) %>%
      filter(location %in% preset_locations) %>%
      mutate(
        location = factor(location, levels = preset_locations),
        location = forcats::fct_rev(location)
      ) %>%
      distinct()
    
    # 跳过无数据的情况
    if (nrow(cause_data) == 0) {
      message(sprintf("跳过可视化 [%s]: 无有效数据", current_cause))
      return()
    }
    
    # 为每个location-measure组合创建标签点
    label_points <- cause_data %>%
      group_by(location, measure) %>%
      reframe(
        time_points = unique(c(segment_start, segment_end))
      ) %>%
      mutate(
        label = as.character(time_points)
      ) %>%
      ungroup()
    
    # 动态计算图表尺寸
    n_measures <- n_distinct(cause_data$measure)
    n_locations <- n_distinct(cause_data$location)
    base_height <- 4 + n_locations * 0.6
    plot_width <- 8 + n_measures * 2
    
    # 创建可视化
    p <- ggplot(cause_data) +
      geom_segment(
        aes(x = segment_start, xend = segment_end,
            y = location, yend = location,
            color = trend),
        linewidth = 4, lineend = "butt", alpha = 0.3
      ) +
      geom_segment(
        aes(x = segment_start, xend = segment_end,
            y = location, yend = location,
            color = trend),
        linewidth = 3, lineend = "butt"
      ) +
      geom_text(
        data = label_points,
        aes(x = time_points, y = location, label = label),
        angle = 45, color = "black", size = 2.5, hjust = -0.2
      ) +
      geom_text(
        aes(x = (segment_start + segment_end)/2, 
            y = location, label = significance),
        color = "white", size = 3, fontface = "bold", vjust = 0.6
      ) +
      facet_wrap(
        ~ measure, 
        ncol = ifelse(n_measures > 2, 2, 1),
        labeller = labeller(measure = label_wrap_gen(15))
      ) +
      scale_x_continuous(
        expand = expansion(add = 1)
      ) +
      scale_color_manual(
        values = c("Increase" = "#C44E52", "Decrease" = "#4C72B0"),
        guide = guide_legend(title = "Trend Direction")
      ) +
      labs(
        x = NULL,
        y = "Location",
        title = current_cause
      ) +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = "#F7F7F7", color = NA),
        panel.spacing = unit(0.5, "lines"),
        plot.title = element_text(face = "bold", size = rel(1.1)),
        strip.text.x = element_text(margin = margin(b = 5))
      )
    
    # 保存图表
    tryCatch({
      ggsave(
        file.path(vis_dir, paste0(cause_safe_name, "_APC.pdf")),
        plot = p,
        width = plot_width,
        height = base_height,
        limitsize = FALSE
      )
      ggsave(
        file.path(vis_dir, paste0(cause_safe_name, "_APC.png")),
        plot = p,
        width = plot_width,
        height = base_height,
        dpi = 300,
        limitsize = FALSE
      )
      ggsave(
        file.path(vis_dir, paste0(cause_safe_name, "_APC.jpg")),
        plot = p,
        width = plot_width,
        height = base_height,
        dpi = 300,
        limitsize = FALSE
      )
    }, error = function(e) {
      message(sprintf("保存失败 [%s]: %s", current_cause, e$message))
    })
  })
}

# 保存工作空间
save.image(file.path(output_main_dir, "joinpoint_analysis_workspace.RData"))
message("分析完成！所有结果已保存在文件夹: ", output_main_dir)