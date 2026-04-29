rm(list = ls())
library(openxlsx)
library(ggplot2)

# ===================== 1. 读取并清洗数据（你已完成，这里整合） =====================
data <- read.xlsx("data.xlsx")
clean_data <- na.omit(data)  # 去除含NA行
cat("✅ 数据清洗完成\n有效数据行数：", nrow(clean_data), "\n")

# ===================== 2. 创建输出文件夹 =====================
if (!dir.exists("output")) dir.create("output")

# ===================== 3. 统一配色（与之前完全一致） =====================
color_region <- c(
  "High-income Asia Pacific" = "#1f77b4",  # 蓝色
  "East Asia" = "#d62728"                   # 红色
)

# ===================== 4. 可视化：风险因素DALYs率双地区对比 =====================
p_risk <- ggplot(clean_data, aes(x = Risk.factor, y = Value, fill = Location)) +
  # 分组柱状图
  geom_col(position = position_dodge(0.9), alpha = 0.85) +
  # 95% UI 误差线
  geom_errorbar(
    aes(ymin = Lower.bound, ymax = Upper.bound),
    position = position_dodge(0.9), width = 0.25, color = "black", linewidth = 0.3
  ) +
  # 配色应用
  scale_fill_manual(values = color_region) +
  # 全英文标签（与前图风格统一）
  labs(
    title = "Diabetes Mellitus DALYs Rate by Risk Factor, 2023",
    subtitle = "Comparison between East Asia and High-income Asia Pacific",
    x = "Risk Factor",
    y = "DALYs per 100,000",
    fill = "Region"
  ) +
  # 统一主题
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8), # 风险因素名长，旋转防重叠
    axis.text.y = element_text(size = 10),
    legend.position = "top"
  )

# ===================== 5. 保存3种格式到output文件夹 =====================
ggsave("output/risk_factor_comparison.jpg", p_risk, width = 8, height = 5, dpi = 300)
ggsave("output/risk_factor_comparison.png", p_risk, width = 8, height = 5, dpi = 300)
ggsave("output/risk_factor_comparison.pdf", p_risk, width = 8, height = 5)

# ===================== 完成提示 =====================
cat("\n🎉 风险因素对比图生成完成！\n📁 文件保存至：output/risk_factor_comparison (jpg/png/pdf)")