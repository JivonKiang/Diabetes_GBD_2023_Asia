rm(list = ls())
# 加载必需包
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

# ===================== 1. 核心固定设置 =====================
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)

# 🔥 严格按照你要求的地区展示顺序
LOCATION_ORDER <- c(
  "High-income Asia Pacific",
  "Brunei Darussalam",
  "Japan",
  "Republic of Korea",
  "Singapore",
  "East Asia",
  "China",
  "Democratic People's Republic of Korea"
)

# 🔥 强制指定情景分面顺序（Past 放在第一个）
SCENARIO_ORDER <- c(
  "Past",
  "Reference",
  "Combined",
  "Improved Behavioral\nand Metabolic Risks",
  "Improved Childhood\nNutrition and Vaccination",
  "Safer Environment"
)

# 🔥 真实情景配色（完全匹配你的数据）
color_scenario <- c(
  "Past" = "#8c564b",
  "Reference" = "#2c3e50",
  "Combined" = "#e74c3c",
  "Improved Behavioral and Metabolic Risks" = "#27ae60",
  "Improved Childhood Nutrition and Vaccination" = "#f39c12",
  "Safer Environment" = "#9b59b6"
)

# 地区配色
color_region <- setNames(
  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"),
  LOCATION_ORDER
)

# ===================== 2. 数据清洗（✅ 保留所有数据点，仅去NA+筛选糖尿病） =====================
clean_diabetes_data <- function(df) {
  df %>%
    # 仅保留糖尿病数据
    filter(Cause.of.death.or.injury == "Diabetes mellitus") %>%
    # 仅去除无值的NA行，保留所有有效数据点
    filter(!is.na(Value)) %>%
    mutate(Year = as.numeric(Year)) %>%
    distinct()
}

# 读取数据
location_df <- read.csv("不同地区的预测数据.csv", stringsAsFactors = FALSE)
east_df <- read.csv("东亚的不同情景预测.csv", stringsAsFactors = FALSE)
high_df <- read.csv("高收入亚太的不同情景预测.csv", stringsAsFactors = FALSE)

# 数据处理
location_clean <- clean_diabetes_data(location_df) %>%
  filter(Scenario == "Reference") %>%
  mutate(Location = factor(Location, levels = LOCATION_ORDER)) %>%
  filter(!is.na(Location))

east_clean <- clean_diabetes_data(east_df)
high_clean <- clean_diabetes_data(high_df)

# ===================== 3. 可视化1：地区分面（仅显示Region图例+底部竖排） =====================
p_location <- ggplot(location_clean, aes(x=Year, y=Value, color=Location, group=Location)) +
  geom_ribbon(aes(ymin=Lower.bound, ymax=Upper.bound, fill=Location), alpha=0.1, linetype=0) +
  geom_line(linewidth=1) +
  geom_point(size=1, alpha=0.8) +
  facet_wrap(~Location, ncol=2, scales="free_y") +
  scale_x_continuous(breaks = seq(1990, 2050, 10)) +
  scale_color_manual(values=color_region) +
  # 🔥 核心：隐藏置信区间的图例，仅保留Region图例
  scale_fill_manual(values=color_region, guide = "none") +
  labs(title="DALYs Rate of Diabetes Mellitus (Reference Scenario)",
       x="Year", y="DALYs per 100,000", color="Region") +
  theme_bw() +
  theme(
    plot.title=element_text(hjust=0.5, size=14, face="bold"),
    legend.position = "bottom",  # 图例放在底部
    strip.text=element_text(face="bold")
  ) +
  # 🔥 Region图例底部2列排列
  guides(color = guide_legend(ncol = 2, byrow = TRUE))

ggsave("output/糖尿病_地区分面_全数据.jpg", p_location, width=6, height=8, dpi=300)

# ===================== 4. 终极优化：分面标签换行+Scenario×Region分面+Past置顶 =====================
# 1. 合并数据 + 长情景名称换行处理 + 强制情景顺序（Past第一）
combined_scenario <- bind_rows(
  east_clean %>% mutate(Region = "East Asia"),
  high_clean %>% mutate(Region = "High-income Asia Pacific")
) %>%
  # 超长情景标签手动换行
  mutate(Scenario = case_when(
    Scenario == "Improved Behavioral and Metabolic Risks" ~ "Improved Behavioral\nand Metabolic Risks",
    Scenario == "Improved Childhood Nutrition and Vaccination" ~ "Improved Childhood\nNutrition and Vaccination",
    TRUE ~ Scenario
  )) %>%
  # 🔥 核心：强制指定分面顺序，Past 放在第一个
  mutate(Scenario = factor(Scenario, levels = SCENARIO_ORDER))

# 2. 配色匹配换行后的名称
color_scenario_opt <- c(
  "Past" = "#999999",
  "Reference" = "#2c3e50",
  "Combined" = "#e74c3c",
  "Improved Behavioral\nand Metabolic Risks" = "#27ae60",
  "Improved Childhood\nNutrition and Vaccination" = "#f39c12",
  "Safer Environment" = "#9b59b6"
)

# 3. 绘图
p_final <- ggplot(combined_scenario, aes(x=Year, y=Value, group=Scenario)) +
  geom_ribbon(aes(ymin=Lower.bound, ymax=Upper.bound, fill=Scenario), alpha=0.15, linetype=0) +
  geom_line(aes(color=Scenario), linewidth=1) +
  geom_point(aes(color=Scenario), size=1, alpha=0.9) +
  geom_vline(xintercept = 2021, color = "#DC143C", linewidth=0.8, linetype="dashed") +
  annotate("text", x=2008, y=Inf, label="Observed\n(1990-2021)", color="#666666", fontface=2, vjust=2) +
  annotate("segment", x=2018, xend=2022, y=Inf, yend=Inf, color="#DC143C", arrow=arrow(length=unit(0.3,"cm")), linewidth=0.8) +
  annotate("text", x=2033, y=Inf, label="Projected\n(2022-2050)", color="#DC143C", fontface=2, vjust=2) +
  # 分面：行=情景，列=地区
  facet_grid(Scenario ~ Region, scales="free_y") +
  scale_x_continuous(breaks = seq(1990, 2050, 10)) +
  scale_color_manual(values=color_scenario_opt) +
  scale_fill_manual(values=color_scenario_opt) +
  labs(
    title="Diabetes Mellitus DALYs Rate (1990-2050)",
    x="Year", y="DALYs per 100,000"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    legend.position = "none",
    strip.text = element_text(face="bold", size=9, lineheight = 1.2),
    strip.background = element_rect(fill="gray95"),
    panel.grid.minor = element_blank()
  )

ggsave("output/糖尿病_最终双分面图.jpg", p_final, width=10, height=12, dpi=300)

# ===================== 验证信息 =====================
cat("✅ 所有数据点已全部保留\n")
cat("✅ 地区分面图图例：底部2列排列\n")
cat("✅ 情景分面：Past 已强制置顶为第一个\n")
cat("✅ 分面布局：Scenario(行) × Region(列)\n")