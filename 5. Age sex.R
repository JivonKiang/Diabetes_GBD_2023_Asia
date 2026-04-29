rm(list = ls())

# Load packages
library(openxlsx)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Read data
data <- read.xlsx("data.xlsx")

# Create output folder
if (!dir.exists("output")) dir.create("output")

# ---------------------------
# Data preparation: Age groups (2023, non-age-standardized)
# ---------------------------
df_age <- data[
  data$Age != "Age-standardized" & data$Year == 2023,
]

age_levels <- c(
  "0-6 days", "7-27 days", "1-5 months", "6-11 months", "12-23 months",
  "2-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years",
  "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years",
  "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years",
  "75-79 years", "80-84 years", "85-89 years", "90-94 years", "95+ years"
)
df_age$Age <- factor(df_age$Age, levels = age_levels)

# ---------------------------
# Data preparation: Trend (age-standardized, 1990-2023)
# ---------------------------
df_trend <- data[data$Age == "Age-standardized", ]
df_trend$Year <- as.numeric(df_trend$Year)

# ---------------------------
# Color settings (per your request)
# ---------------------------
# Region colors: High-income Asia Pacific = blue, East Asia = red
color_region <- c(
  "High-income Asia Pacific" = "#1f77b4",
  "East Asia" = "#d62728"
)

# Sex colors: Male = blue, Female = red, Both = orange
color_sex <- c(
  "Male" = "#1f77b4",
  "Female" = "#d62728",
  "Both" = "#ff7f0e"
)

# ---------------------------
# Plot 1: Age-group DALYs by region (bar + 95% UI error bars)
# ---------------------------
p_age <- ggplot(df_age, aes(x = Age, y = Value, fill = Location)) +
  geom_col(position = position_dodge(0.9), alpha = 0.85) +
  geom_errorbar(
    aes(ymin = Lower.bound, ymax = Upper.bound),
    position = position_dodge(0.9), width = 0.25, color = "black", linewidth = 0.3
  ) +
  scale_fill_manual(values = color_region) +
  labs(
    title = "Diabetes Mellitus DALYs Rate by Age Group, 2023",
    subtitle = "Comparison between East Asia and High-income Asia Pacific",
    x = "Age Group",
    y = "DALYs per 100,000",
    fill = "Region"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "top"
  )

# Save age plot
ggsave("output/age_group_comparison.jpg", p_age, width = 8, height = 5, dpi = 300)
ggsave("output/age_group_comparison.png", p_age, width = 8, height = 5, dpi = 300)
ggsave("output/age_group_comparison.pdf", p_age, width = 8, height = 5)

# ---------------------------
# Plot 2: Trend for East Asia (line + 95% UI ribbon)
# ---------------------------
p_east <- ggplot(subset(df_trend, Location == "East Asia"),
                 aes(x = Year, y = Value, color = Sex)) +
  geom_ribbon(
    aes(ymin = Lower.bound, ymax = Upper.bound, fill = Sex),
    alpha = 0.2, linetype = 0
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = color_sex) +
  scale_fill_manual(values = color_sex) +
  labs(
    title = "Age-standardized DALYs Rate of Diabetes Mellitus",
    subtitle = "East Asia, 1990–2023",
    x = "Year",
    y = "DALYs per 100,000",
    color = "Sex",
    fill = "95% UI"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top"
  )

# Save East Asia plot
ggsave("output/trend_east_asia.jpg", p_east, width = 8, height = 5, dpi = 300)
ggsave("output/trend_east_asia.png", p_east, width = 8, height = 5, dpi = 300)
ggsave("output/trend_east_asia.pdf", p_east, width = 8, height = 5)

# ---------------------------
# Plot 3: Trend for High-income Asia Pacific (line + 95% UI ribbon)
# ---------------------------
p_high <- ggplot(subset(df_trend, Location == "High-income Asia Pacific"),
                 aes(x = Year, y = Value, color = Sex)) +
  geom_ribbon(
    aes(ymin = Lower.bound, ymax = Upper.bound, fill = Sex),
    alpha = 0.2, linetype = 0
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = color_sex) +
  scale_fill_manual(values = color_sex) +
  labs(
    title = "Age-standardized DALYs Rate of Diabetes Mellitus",
    subtitle = "High-income Asia Pacific, 1990–2023",
    x = "Year",
    y = "DALYs per 100,000",
    color = "Sex",
    fill = "95% UI"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top"
  )

# Save High-income Asia Pacific plot
ggsave("output/trend_high_income_asia.jpg", p_high, width = 8, height = 5, dpi = 300)
ggsave("output/trend_high_income_asia.png", p_high, width = 8, height = 5, dpi = 300)
ggsave("output/trend_high_income_asia.pdf", p_high, width = 8, height = 5)

# ---------------------------
# Finish message
# ---------------------------
cat("✅ All plots generated and saved to /output folder:\n")
cat("- age_group_comparison (jpg/png/pdf)\n")
cat("- trend_east_asia (jpg/png/pdf)\n")
cat("- trend_high_income_asia (jpg/png/pdf)\n")