library(tidyverse)
library(vroom)
library(lubridate)

file_path <- "output.csv"
df <- read.csv(file_path,sep = ",",fileEncoding = "UTF-8-BOM")

transfered <- df |>
  filter(is.na(检查) | 检查 == "") |>
  mutate(
    p1 = str_c(项目名称.1, as.character(项目工时.1), sep = ":"),
    p2 = str_c(项目名称.2, as.character(项目工时.2), sep = ":"),
    p3 = str_c(项目名称.3, as.character(项目工时.3), sep = ":")
  ) |>
  select(-c(项目名称.1, 项目工时.1, 项目名称.2, 项目工时.2, 项目名称.3, 项目工时.3)) |> 
  pivot_longer(cols = c(p1, p2, p3), names_to = "del", values_to = "项目工时") |>
  select(-del) |>
  separate(项目工时, into = c("项目名称", "项目工时"), sep = ":", fill = "right", extra = "merge") |>
  mutate(
    项目名称 = str_trim(项目名称),
    项目工时 = as.numeric(项目工时),
    填报日期 = as_datetime(as.numeric(填报日期)/1000, tz = "UTC") |> as_date()
  ) |>
  filter(!is.na(项目名称), 项目名称 != "") |>
  select(-c(检查,record_id,提交人,提交时间,自动编号)) |>
  relocate(填报日期, 姓名)

project_hour <- transfered |>
  mutate(
    Month = as.character(month(填报日期)),
    Quarter = as.character(quarter(填报日期)),
    Year = as.character(year(填报日期))
  ) |>
  group_by(Month, 项目名称) |>
  summarise(总工时 = sum(项目工时, na.rm = TRUE), .groups = "drop")

project_hour |>
  ggplot(aes(x = 项目名称, y = 总工时)) +
  geom_point()

user_hour <- transfered |>
  mutate(
    Month = as.character(month(填报日期)),
    Quarter = as.character(quarter(填报日期)),
    Year = as.character(year(填报日期))
  ) |>
  group_by(Month, 姓名) |>
  summarise(总工时 = sum(项目工时, na.rm = TRUE), .groups = "drop")

user_hour |>
  ggplot(aes(x = 姓名, y = 总工时)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

invisible(list(project_hour = project_hour, user_hour = user_hour))

# 基于已有 user_hour 对象构造可复用的 ggplot 分析图
user_hour_viz <- (function(df){
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(scales)

  # 1. 准备数据
  df_prep <- df |>
    mutate(
      Month = as.integer(as.character(Month)),        # 确保是整数（若原为字符）
      Month = factor(Month, levels = sort(unique(Month))) 
    )

  # 按总工时对姓名排序（总计）
  name_order <- df_prep |>
    group_by(姓名) |>
    summarise(total = sum(总工时, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(total)) |>
    pull(姓名)

  df_prep <- df_prep |>
    mutate(姓名 = factor(姓名, levels = name_order))

  # top users 表（可选）
  top_users <- name_order[seq_len(min(10, length(name_order)))]

  # 2. 图1：分面点图（每月各用户总工时分布）
  p1 <- ggplot(df_prep, aes(x = 姓名, y = 总工时)) +
    geom_point(alpha = 0.8, size = 2, colour = "#2c7fb8") +
    facet_wrap(~ Month, scales = "free_x", nrow = 1) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "每月各用户总工时（按月分面）", x = "姓名", y = "总工时")

  # 3. 图2：按月份堆叠条形（每用户每月贡献）—— dodge 版本注释可切换为 position = "stack"
  p2 <- ggplot(df_prep, aes(x = 姓名, y = 总工时, fill = Month)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.8) +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "每月用户工时（按月份着色，DODGE）", x = "姓名", y = "总工时", fill = "Month")

  # 4. 图3：热力图 用户 x 月份
  df_tile <- df_prep |>
    complete(姓名, Month, fill = list(总工时 = 0))

  p3 <- ggplot(df_tile, aes(x = Month, y = 姓名, fill = 总工时)) +
    geom_tile(colour = "grey90") +
    scale_fill_viridis_c(option = "magma", labels = comma) +
    theme_minimal() +
    labs(title = "用户-月份工时热力图", x = "Month", y = "姓名", fill = "总工时") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

  base_font_size <- 6

my_theme <- theme_minimal(base_size = base_font_size) +
  theme(
    plot.title   = element_text(size = base_font_size + 4, face = "bold"),
    axis.title   = element_text(size = base_font_size + 2),
    axis.text    = element_text(size = base_font_size),
    axis.text.x  = element_text(angle = 0, hjust = 1, size = base_font_size - 1),
    legend.title = element_text(size = base_font_size),
    legend.text  = element_text(size = base_font_size - 1),
    strip.text   = element_text(size = base_font_size), # facet labels
    plot.caption = element_text(size = base_font_size - 2)
  )

# 将主题加到已创建的 p1 / p2 / p3
p1 <- p1 + my_theme
p2 <- p2 + my_theme
p3 <- p3 + my_theme
  
theme_set(my_theme)
  
  # 返回有用对象，便于后续保存或在交互式会话中显示
  list(
    plots = list(by_month_facet = p1, monthly_dodge = p2, heatmap = p3),
    top_users = top_users,
    data = df_prep
  )
})(user_hour)

# ...existing code...
# 基于 project_hour 构造可复用的 ggplot 分析图
project_hour_viz <- (function(df){
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(scales)
  library(viridis)

  # 准备数据：确保 Month 为有序因子，按总工时排序项目
  df_prep <- df |>
    mutate(
      Month = as.integer(as.character(Month)),
      Month = factor(Month, levels = sort(unique(Month)))
    )

  project_order <- df_prep |>
    group_by(项目名称) |>
    summarise(total = sum(总工时, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(total)) |>
    pull(项目名称)

  df_prep <- df_prep |>
    mutate(项目名称 = factor(项目名称, levels = project_order))

  top_n <- 10
  top_projects <- project_order[seq_len(min(top_n, length(project_order)))]

  # 图1：横向条形 — 项目总工时（所有月份合计）
  p1_data <- df_prep |>
    group_by(项目名称) |>
    summarise(总工时 = sum(总工时, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(总工时))

  p1 <- ggplot(p1_data, aes(x = fct_reorder(项目名称, 总工时), y = 总工时)) +
    geom_col(fill = "#2b8cbe", width = 0.7) +
    coord_flip() +
    labs(title = "项目总工时排名", x = "项目名称", y = "总工时") +
    theme_minimal()

  # 图2：前 N 项目的月度趋势（折线）
  p2_data <- df_prep |>
    filter(项目名称 %in% top_projects) |>
    group_by(Month, 项目名称) |>
    summarise(总工时 = sum(总工时, na.rm = TRUE), .groups = "drop")

  p2 <- ggplot(p2_data, aes(x = Month, y = 总工时, color = 项目名称, group = 项目名称)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = paste0("前 ", length(top_projects), " 项目月度工时趋势"), x = "Month", y = "总工时", color = "项目") +
    theme_minimal() +
    theme(legend.position = "right")

  # 图3：项目 x 月份 热力图（按项目排序）
  df_tile <- df_prep |>
    complete(项目名称, Month, fill = list(总工时 = 0))

  p3 <- ggplot(df_tile, aes(x = Month, y = 项目名称, fill = 总工时)) +
    geom_tile(colour = "grey90") +
    scale_fill_viridis_c(option = "magma", labels = comma) +
    labs(title = "项目-月份工时热力图", x = "Month", y = "项目名称", fill = "总工时") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

  # 统一主题（可调整 base_font_size）
  base_font_size <- 5
  my_theme <- theme_minimal(base_size = base_font_size) +
    theme(
      plot.title = element_text(size = base_font_size + 4, face = "bold"),
      axis.title = element_text(size = base_font_size + 2),
      axis.text = element_text(size = base_font_size),
      legend.title = element_text(size = base_font_size),
      legend.text = element_text(size = base_font_size - 1),
      strip.text = element_text(size = base_font_size)
    )

  p1 <- p1 + my_theme
  p2 <- p2 + my_theme
  p3 <- p3 + my_theme

  list(
    plots = list(rank_bar = p1, top_trends = p2, heatmap = p3),
    top_projects = top_projects,
    data = df_prep
  )
})(project_hour)
# ...existing code...