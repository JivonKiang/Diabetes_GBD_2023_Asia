# ===================== 1. 加载依赖包 =====================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, sf, rnaturalearth, patchwork,
  ggspatial, scales, RColorBrewer, openxlsx
)

# ===================== 2. 自定义配色 =====================
colors_pal <- rev(brewer.pal(8, 'RdYlBu'))

# ===================== 3. 创建输出文件夹 =====================
dir.create("output", showWarnings = FALSE)

# ===================== 4. 读取Excel数据 =====================
df <- read.xlsx("combined_data.xlsx", sheet = "Sheet2")
df <- df[, c(4,6,10,12,15,16,17,18)]
colnames(df) <- c("measure","location","age_name","cause","year","val","lower","upper")

# ===================== 5. 计算MIR及其95%UI =====================
df_mir <- df %>%
  select(measure, location, val, lower, upper) %>%
  pivot_wider(names_from = measure, values_from = c(val, lower, upper)) %>%
  mutate(
    val_MIR   = val_Deaths / val_Incidence,
    lower_MIR = lower_Deaths / lower_Incidence,
    upper_MIR = upper_Deaths / upper_Incidence
  ) %>%
  select(location, val_MIR, lower_MIR, upper_MIR) %>%
  pivot_longer(cols = c(val_MIR, lower_MIR, upper_MIR),
               names_to = c(".value", "measure"),
               names_sep = "_") %>%
  mutate(age_name = "Age-standardized", cause = "Diabetes mellitus", year = 2023)

df_all <- bind_rows(df, df_mir)

# ===================== 6. 地图数据预处理 =====================
country_match <- tibble(
  location = c("Singapore", "Republic of Korea", "Japan",
               "Democratic People's Republic of Korea", "China", "Brunei Darussalam"),
  map_name = c("Singapore", "South Korea", "Japan",
               "North Korea", "China", "Brunei")
)

world_map <- ne_countries(scale = "medium", returnclass = "sf")
asia_base <- world_map %>%
  filter(continent == "Asia") %>%
  select(map_name = name, geometry)

# ✅ 修复笔误：country → country_match
df_geo <- df_all %>%
  left_join(country_match, by = "location") %>%
  left_join(world_map %>% select(map_name = name, geometry), by = "map_name") %>%
  st_sf()

# ===================== 7. 核心绘图函数（美学优化版） =====================
visualize_indicator <- function(indicator, label_threshold) {
  plot_data <- df_geo %>% filter(measure == indicator)
  
  # ===================== 【核心修改】相对百分比定位标签（无绝对距离） =====================
  bar_data <- plot_data %>%
    arrange(desc(val)) %>%
    mutate(location = fct_reorder(location, val)) %>%
    # 计算柱子占最大值的比例
    mutate(prop = val / max(val, na.rm = TRUE),
           # 标签格式
           label = paste0(round(val,2), " (", round(lower,2), ", ", round(upper,2), ")"),
           # 🔥 核心：标签X坐标（相对柱子位置，无绝对偏移）
           # >88% / 20%-88% → X=0（柱子最左侧，抵底部）
           # <20% → X=val（柱子最右侧外部）
           x_pos = case_when(
             prop < 0.2 ~ val,    # 小于20%：柱子右侧外部
             TRUE ~ 0             # 大于等于20%：柱子左侧底部
           ),
           # 全部统一左对齐（纯相对位置，无绝对距离）
           hjust = 0,
           # 颜色规则
           color = case_when(
             prop > 0.88 ~ "white",
             TRUE ~ "black"
           ))
  
  # 统一全局边距：轻微留白，美观协调
  global_margin <- theme(plot.margin = margin(3,3,3,3), legend.position = "none",
                         axis.text = element_blank(), axis.title = element_blank())
  
  # 1. 扩大版主地图（东亚+东南亚）+ 箭头文字置于海洋空白区
  main_map <- ggplot() +
    geom_sf(data = asia_base, fill = "#E0E0E0", color = "black", linewidth = 0.4) +
    geom_sf(data = plot_data, aes(fill = val), color = "black", linewidth = 0.6) +
    # 新加坡箭头+文字：移至南部海洋空白区
    annotate("segment", x = 104, y = 0, xend = 103.8, yend = 1.2,
             arrow = arrow(length = unit(0.25, "cm")), color = "black", linewidth = 0.7) +
    annotate("text", x = 104, y = -1, label = "Singapore", fontface = "bold", size = 4) +
    # 文莱箭头+文字：移至东部海洋空白区
    annotate("segment", x = 120, y = 3, xend = 114.9, yend = 4.5,
             arrow = arrow(length = unit(0.25, "cm")), color = "black", linewidth = 0.7) +
    annotate("text", x = 121, y = 2, label = "Brunei", fontface = "bold", size = 4) +
    coord_sf(xlim = c(90, 145), ylim = c(-5, 50)) +
    scale_fill_gradientn(colors = colors_pal, labels = comma) +
    labs(fill = "Value", caption = "Age-standardized") +
    ggtitle("East & High-Income Asia Map") +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.5, face="bold", size=13),
          plot.margin = margin(3,3,3,3), legend.position = "none")
  
  # 2. 新加坡放大图（带统一边距）
  sg_zoom <- ggplot() +
    geom_sf(data = asia_base, fill = "#E0E0E0", color = "black", linewidth = 0.4) +
    geom_sf(data = plot_data, aes(fill = val), color = "black", linewidth = 0.6) +
    coord_sf(xlim = c(102, 106), ylim = c(0, 3)) +
    scale_fill_gradientn(colors = colors_pal) +
    ggtitle("Singapore (Zoom)") +
    theme_minimal() + global_margin +
    theme(plot.title = element_text(hjust=0.5, face="bold", size=11))
  
  # 3. 文莱放大图（带统一边距，与上图自然形成微间隙）
  bn_zoom <- ggplot() +
    geom_sf(data = asia_base, fill = "#E0E0E0", color = "black", linewidth = 0.4) +
    geom_sf(data = plot_data, aes(fill = val), color = "black", linewidth = 0.6) +
    coord_sf(xlim = c(112, 118), ylim = c(2, 7)) +
    scale_fill_gradientn(colors = colors_pal) +
    ggtitle("Brunei (Zoom)") +
    theme_minimal() + global_margin +
    theme(plot.title = element_text(hjust=0.5, face="bold", size=11))
  
  # 4. 水平柱状图（统一边距+等高）
  bar_plot <- ggplot(bar_data, aes(x = val, y = location)) +
    geom_col(aes(fill = val), color = "black", linewidth = 0.3) +
    geom_text(aes(x = x_pos, label = label, hjust = hjust, color = color), size = 3.1, fontface = "bold") +
    scale_fill_gradientn(colors = colors_pal, labels = comma) +
    scale_color_identity() +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.25))) +
    labs(x = "Value", y = "Country", fill = "Value") +
    ggtitle("Descending Bar Chart (95% UI)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.5, face="bold", size=13),
          legend.position = "bottom",
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.1, "cm"),
          legend.margin = margin(0,0,0,0),
          plot.margin = margin(3,3,3,3),
          axis.text.y = element_text(size=10))
  
  # ---------------------- 黄金比例布局 ----------------------
  middle_panel <- sg_zoom / bn_zoom + plot_layout(heights = c(1,1))
  combined_plot <- main_map + middle_panel + bar_plot +
    plot_layout(ncol = 3, widths = c(2.8, 1.2, 2.8))
  
  # 总标题
  combined_plot <- combined_plot +
    plot_annotation(title = paste0(indicator, " - Diabetes Mellitus (2023)"),
                    theme = theme(plot.title = element_text(hjust=0.5, size=16, face="bold")))
  
  # 保存高清图
  ggsave(paste0("output/", indicator, ".png"), combined_plot,
         width=10, height=4, dpi=300, bg="white")
  return(combined_plot)
}

# ===================== 8. 批量出图（自定义标签阈值） =====================
visualize_indicator("Deaths",      label_threshold = 10)
visualize_indicator("Incidence",   label_threshold = 200)
visualize_indicator("DALYs",       label_threshold = 600)
visualize_indicator("Prevalence",  label_threshold = 5000)
visualize_indicator("MIR",         label_threshold = 0.05)

cat("✅ 美学优化完成！图表已保存至 output 文件夹\n")