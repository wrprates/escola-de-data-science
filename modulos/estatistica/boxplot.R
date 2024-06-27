x <- c(1, 1, 2, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 8, 9, 13, 14, 21, 76)
quantile(x)
boxplot(x)
IQR(x)

library(highcharter)

df <- data.frame("x" = x)

# No outliers
dat <- data_to_boxplot(df, x)
highchart() |>
  hc_xAxis(type = "category") |>
  hc_add_series_list(dat)

# Com outliers
dat <- data_to_boxplot(df, x, add_outliers = TRUE)
highchart() |>
  hc_xAxis(type = "category") |>
  hc_add_series_list(dat)

####
# Boxplot com echarts4r mais de um grupo
library(echarts4r)

data <- readr::read_csv(
  "https://raw.githubusercontent.com/wrprates/open-data/master/ibm_hr_emplyee_attrition.csv"
)

sorted_data <- data |>
  dplyr::group_by(JobRole) |>
  dplyr::mutate(median_income = median(MonthlyIncome, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::mutate(JobRole = factor(JobRole, levels = JobRole[order(median_income)]))

median_income_df <- data %>%
  group_by(JobRole) %>%
  summarize(median_income = median(MonthlyIncome, na.rm = TRUE))

# Arrange JobRole by median income and create a factor
sorted_data <- data |>
  left_join(median_income_df, by = "JobRole") |>
  # mutate(JobRole = factor(JobRole, levels = median_income_df$JobRole[order(median_income_df$median_income)])) |>
  mutate(JobRole = factor(JobRole, levels = rev(median_income_df$JobRole[order(median_income_df$median_income)]))) |>
  arrange(JobRole)

sorted_data |>
  group_by(JobRole) |>
  e_chart() |>
  e_boxplot(MonthlyIncome) |>
  e_color(color = "#4292b5") |>
  e_tooltip() |>
  e_title(
    text = "Renda mensal por JobRole",
    subtext = "Em milhares de USD."
  )
