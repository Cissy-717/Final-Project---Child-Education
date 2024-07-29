## Early Childhood Education Program

# 1 Install and Load Packages 
library(tidyverse)
library(janitor)
library(ggplot2)
install.packages("RColorBrewer")
library(RColorBrewer)
library(scales)

# 2.1 Load and Clean Data (Citywide Level)

citywide <- read.csv(file = "demographic-snapshot-2018-19-to-2022-23-(public) - Citywide.csv")

citywide_clean <- citywide %>% 
  clean_names() %>% 
  rename_with(~str_replace_all(., "^x_", ""), everything())

unique(citywide_clean$year)

citywide_clean <- citywide_clean %>% 
  mutate(year = as.character(year))
  
citywide_clean <- citywide_clean %>% 
  mutate(across(c(grade_3k, grade_pk_half_day_full_day, grade_k), ~as.numeric(gsub(",", "", .))))

str(citywide_clean)

citywide_clean <- citywide_clean %>%
  select(-ends_with(".1"))

# 2.2 Aggregate Enrollment Data

aggregated_citywide <- citywide_clean %>%
  select(year, grade_3k, grade_pk_half_day_full_day, grade_k) %>%
  group_by(year) %>%
  summarise(
    total_3k = sum(grade_3k, na.rm = TRUE),
    total_pk = sum(grade_pk_half_day_full_day, na.rm = TRUE),
    total_k = sum(grade_k, na.rm = TRUE)
  )

aggregated_long_citywide <- aggregated_citywide %>%
  pivot_longer(cols = starts_with("total"), names_to = "grade", values_to = "enrollment")

# 2.3 Plot Enrollment Trends

ggplot(aggregated_long_citywide, aes(x = year, y = enrollment, color = grade, group = grade)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Early Education Enrollment Trends Over Time",
       x = "Year",
       y = "Enrollment",
       color = "Grade")

ggsave("Early_Education_Enrollment_Trends_Over_Time.jpg", plot = last_plot(), width = 8, height = 6)

write_csv(aggregated_citywide, "aggregated_18-23.csv")

yearly_totals <- aggregated_citywide %>% 
  group_by(year) %>% 
  summarise(total_enrollment = sum(total_3k, total_pk, total_k, na.rm = TRUE))

ggplot(yearly_totals, aes(x = year, y = total_enrollment, group = 1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 170000, by = 15000), limits = c(0, 170000)) +
  labs(title = "Total Enrollment Trends Over Years",
       x = "Year",
       y = "Total Enrollment")

ggsave("Total Enrollment Trends Over Years.jpg", plot = last_plot(), width = 8, height = 6)

# 3.1 Process Borough Data

borough <- read.csv(file = "demographic-snapshot-2018-19-to-2022-23-(public) - Borough.csv")
district <- read_csv(file = "demographic-snapshot-2018-19-to-2022-23-(public) - District.csv")

borough_clean <- borough %>% 
  clean_names() %>% 
  rename_with(~str_replace_all(., "^x_", ""), everything())
district_clean <- district %>% 
  clean_names() %>% 
  rename_with(~str_replace_all(., "^x_", ""), everything())

borough_clean <- borough_clean %>%
  mutate(across(c(grade_3k, grade_pk_half_day_full_day, grade_k), ~as.numeric(gsub(",", "", .))))

district_clean <- district_clean %>%
  mutate(across(c(grade_3k, grade_pk_half_day_full_day, grade_k), ~as.numeric(gsub(",", "", .))))

borough_clean <- borough_clean %>%
  mutate(year = as.factor(year))
district_clean <- district_clean %>%
  mutate(year = as.factor(year))

# 3.2 Aggregate Borough Enrollment Data

borough_aggregated <- borough_clean %>%
  select(year, borough, grade_3k, grade_pk_half_day_full_day, grade_k) %>%
  group_by(year, borough) %>%
  summarise(
    total_3k = sum(grade_3k, na.rm = TRUE),
    total_pk = sum(grade_pk_half_day_full_day, na.rm = TRUE),
    total_k = sum(grade_k, na.rm = TRUE)
  )

write_csv(borough_aggregated, "borough_aggregated.csv")

# 3.3 Plot Borough Enrollment Trends

ggplot(borough_aggregated, aes(x = year, y = total_3k, color = borough, group = borough)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "3-K Enrollment Trends by Borough",
       x = "Year",
       y = "Enrollment",
       color = "Borough")
ggsave("3-K_Enrollment_Trends_by_Borough.jpg", plot = last_plot(), width = 8, height = 6)

ggplot(borough_aggregated, aes(x = year, y = total_pk, color = borough, group = borough)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Pre-K Enrollment Trends by Borough",
       x = "Year",
       y = "Enrollment",
       color = "Borough")

ggsave("Pre-K_Enrollment_Trends_by_Borough.jpg", plot = last_plot(), width = 8, height = 6)

ggplot(borough_aggregated, aes(x = year, y = total_k, color = borough, group = borough)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Kindergarten Enrollment Trends by Borough",
       x = "Year",
       y = "Enrollment",
       color = "Borough")

ggsave("Kindergarten_Enrollment_Trends_by_Borough.jpg", plot = last_plot(), width = 8, height = 6)

yearly_totals_borough <- borough_aggregated %>% 
  group_by(year) %>% 
  summarise(total_enrollment = sum(total_3k, total_pk, total_k, na.rm = TRUE))

# 3.4 Calculate and Plot Average Enrollment

borough_summary <- borough_aggregated %>%
  group_by(borough) %>%
  summarise(
    mean_3k = mean(total_3k, na.rm = TRUE),
    mean_pk = mean(total_pk, na.rm = TRUE),
    mean_k = mean(total_k, na.rm = TRUE)
  ) %>% 
  mutate(total_enrollment = mean_3k + mean_pk + mean_k)

write_csv(borough_summary, "borough_summary.csv")

ggplot(borough_summary, aes(x = borough, y = total_enrollment, fill = borough)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Five-Year Average of Total Enrolment by Borough",
       x = "Borough",
       y = "Total Enrollment")

ggsave("Five-Year Average of Total Enrolment by Borough.jpg", plot = last_plot(), width = 8, height = 6)

common_y_limits <- c(0, 25000)

ggplot(borough_summary, aes(x = borough, y = mean_3k, fill = borough)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average 3-K Enrollment by Borough",
       x = "Borough",
       y = "Average Enrollment") +
  ylim(common_y_limits)


ggsave("Average 3-K Enrollment by Borough.jpg", plot = last_plot(), width = 8, height = 6)

ggplot(borough_summary, aes(x = borough, y = mean_pk, fill = borough)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Pre-K Enrollment by Borough",
       x = "Borough",
       y = "Average Enrollment") +
  ylim(common_y_limits)
ggsave("Average Pre-K Enrollment by Borough.jpg", plot = last_plot(), width = 8, height = 6)

ggplot(borough_summary, aes(x = borough, y = mean_k, fill = borough)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Kindergarten Enrollment by Borough",
       x = "Borough",
       y = "Average Enrollment") +
  ylim(common_y_limits)
ggsave("Average Kindergarten Enrollment by Borough.jpg", plot = last_plot(), width = 8, height = 6)

# 4 Analyze Child Population and Enrollment Rates

child_population <- read.csv(file = "Population of Children Under 5.csv")

clean_child_population <- child_population %>% 
  clean_names()

borough_child_population <- clean_child_population %>% 
  filter(time_frame == 2020 & location %in% c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"))

ggplot(borough_child_population, aes(x = location, y = data, fill = location)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Child Population by Borough in 2020",
       x = "Borough",
       y = "Child Population")
ggsave("Child Population by Borough in 2020.jpg", plot = last_plot(), width = 8, height = 6)

borough_2020 <- borough_aggregated %>% filter(year == "2020-21")

merged_2020 <- left_join(borough_2020, borough_child_population, by = c("borough" = "location"))

merged_2020 <- merged_2020 %>% 
  mutate(enrollment_rate = (total_3k + total_pk + total_k) / data)

ggplot(merged_2020, aes(x = borough, y = enrollment_rate, fill = borough)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "2020 Enrollment Rate by Borough",
       x = "Borough",
       y = "Enrollment Rate (%)")

ggsave("2020 Enrollment Rate by Borough.jpg", plot = last_plot(), width = 8, height = 6)

write_csv(merged_2020, "child_enrollment_rate.csv")

library(viridis)

custom_palette <- colorRampPalette(c("#FF5733", "#33FF57", "#3357FF", "#FF33B5", "#F3FF33", "#33FFF5", "#F533FF", "#FFD733", "#A733FF", "#FF3339", "#33FFA7"))(65)

# 5.1 Analyze Poverty Rates

poverty_data <- read.csv(file = "In Households Below 200% of the Federal Poverty Level.csv")  

poverty_2019 <- poverty_data %>% 
  filter(TimeFrame == 2019 & DataFormat == "Percent") %>% 
  select(Location, poverty_rate = Data)

str(poverty_2019)

write_csv(poverty_2019, "poverty_rate_2019.csv")

poverty_2019 %>% 
  ggplot(aes(x = reorder(Location, poverty_rate), y = poverty_rate, fill = Location)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = custom_palette) +
  labs(title = "Poverty Rate by Location",
       x = "Location",
       y = "Poverty Rate (%)") +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("Poverty Rate by Location.jpg", plot = last_plot(), width = 14, height = 10)


race_citywide_clean <- citywide_clean %>%
  mutate(across(c(total_enrollment, grade_3k, grade_pk_half_day_full_day, grade_k, 
                  asian, black, hispanic, white), 
                ~as.numeric(gsub(",", "", .))),
         year = as.character(year))
print(race_citywide_clean)

race_citywide <- race_citywide_clean %>% 
  mutate(asian_total = asian * (grade_3k + grade_pk_half_day_full_day + grade_k) / total_enrollment,
         black_total = black * (grade_3k + grade_pk_half_day_full_day + grade_k) / total_enrollment,
         hispanic_total = hispanic * (grade_3k + grade_pk_half_day_full_day + grade_k) / total_enrollment,
         white_total = white * (grade_3k + grade_pk_half_day_full_day + grade_k) / total_enrollment)

print(race_citywide)
write_csv(race_citywide, "race_citywide.csv")
  
race_long_citywide <- race_citywide %>%
  select(year, asian_total, black_total, hispanic_total, white_total) %>%
  pivot_longer(cols = c(asian_total, black_total, hispanic_total, white_total), 
               names_to = "race", 
               values_to = "enrollment")

ggplot(race_long_citywide, aes(x = year, y = enrollment, color = race, group = race)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_color_manual(values = c("asian_total" = "purple",
                                "black_total" = "pink",
                                "hispanic_total" = "orange",
                                "white_total" = "skyblue")) +
  scale_y_continuous(limits = c(0, max(race_long_citywide$enrollment, na.rm = TRUE))) +
  labs(title = "Trends of Racial Enrollment Numbers in Early Education (Citywide)",
       x = "Year",
       y = "Enrollment Numbers",
       color = "Race") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
    )

ggsave("Trends of Racial Enrollment Numbers in Early Education (Citywide).jpg", plot = last_plot(), width = 14, height = 10)

race_citywide_clean <- citywide_clean %>% 
  clean_names() %>% 
  rename_with(~str_replace_all(., "^x_", ""), everything()) %>%
  mutate(across(c(grade_3k, grade_pk_half_day_full_day, grade_k, 
                  asian, black, hispanic, multi_racial, native_american, white), 
                ~as.numeric(gsub(",", "", .))),
         year = as.character(year))

race_2020_21 <- race_citywide_clean %>% 
  filter(year == "2020-21")

race_2020_21 <- race_2020_21 %>%
  mutate(total_students = asian + black + hispanic + multi_racial + native_american + white,
         percent_asian = asian / total_students * 100,
         percent_black = black / total_students * 100,
         percent_hispanic = hispanic / total_students * 100,
         percent_multi_racial = multi_racial / total_students * 100,
         percent_native_american = native_american / total_students * 100,
         percent_white = white / total_students * 100)

race_long_2020_21 <- race_2020_21 %>%
  select(grade_3k, grade_pk_half_day_full_day, grade_k, 
         percent_asian, percent_black, percent_hispanic, percent_multi_racial, 
         percent_native_american, percent_white) %>%
  pivot_longer(cols = starts_with("percent_"), 
               names_to = "race", 
               values_to = "percentage")

ggplot(race_long_2020_21, aes(x = race, y = percentage, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Racial Enrollment Percentages in Early Education (2020-21)",
       x = "Race",
       y = "Percentage",
       fill = "Race") +
  theme(legend.position = "none")

ggsave("Racial Enrollment Percentages in Early Education (2020-21).jpg", plot = last_plot(), width = 14, height = 10)

# 5.2 Descriptive Statistics, ANOVA Analysis, and Regression

borough_clean <- borough_clean %>%
  mutate(
    asian_1 = as.numeric(gsub("%", "", asian_1)),
    black_1 = as.numeric(gsub("%", "", black_1)),
    hispanic_1 = as.numeric(gsub("%", "", hispanic_1)),
    white_1 = as.numeric(gsub("%", "", white_1))
  )
  
borough_clean <- borough_clean %>%
  mutate(
    total_asian_enrollment = grade_k * (asian_1 / 100) + grade_pk_half_day_full_day * (asian_1 / 100) + grade_3k * (asian_1 / 100),
    total_black_enrollment = grade_k * (black_1 / 100) + grade_pk_half_day_full_day * (black_1 / 100) + grade_3k * (black_1 / 100),
    total_hispanic_enrollment = grade_k * (hispanic_1 / 100) + grade_pk_half_day_full_day * (hispanic_1 / 100) + grade_3k * (hispanic_1 / 100),
    total_white_enrollment = grade_k * (white_1 / 100) + grade_pk_half_day_full_day * (white_1 / 100) + grade_3k * (white_1 / 100)
  )

borough_child_summary <- borough_clean %>% 
  select(year, borough, total_asian_enrollment, total_black_enrollment, total_hispanic_enrollment, total_white_enrollment)

aggregated_child_borough <- borough_child_summary %>%
  group_by(borough) %>%
  summarize(
    mean_asian_enrollment = mean(total_asian_enrollment, na.rm = TRUE),
    mean_black_enrollment = mean(total_black_enrollment, na.rm = TRUE),
    mean_hispanic_enrollment = mean(total_hispanic_enrollment, na.rm = TRUE),
    mean_white_enrollment = mean(total_white_enrollment, na.rm = TRUE)
  )

write_csv(aggregated_child_borough, "aggregated_child_borough.csv")

long_data <- aggregated_child_borough %>%
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "Race",
    values_to = "Enrollment",
    names_prefix = "mean_"
  )

ggplot(long_data, aes(x = borough, y = Enrollment, fill = Race)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Enrollment by Borough and Race", x = "Borough", y = "Average Enrollment") +
  theme_minimal() +
  scale_fill_manual(values = c("asian_enrollment" = "purple", "black_enrollment" = "violet", "hispanic_enrollment" = "orange", "white_enrollment" = "skyblue")) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

ggsave("Average Enrollment by Borough and Race.jpg", plot = last_plot(), width = 14, height = 10)

desc_stats_race <- borough_clean %>% 
  select(borough, total_asian_enrollment, total_black_enrollment, total_hispanic_enrollment, total_white_enrollment) %>% 
  group_by(borough) %>% 
  summarise(
    mean_asian = mean(total_asian_enrollment, na.rm = TRUE),
    sd_asian = sd(total_asian_enrollment, na.rm = TRUE),
    mean_black = mean(total_black_enrollment, na.rm = TRUE),
    sd_black = sd(total_black_enrollment, na.rm = TRUE),
    mean_hispanic = mean(total_hispanic_enrollment, na.rm = TRUE),
    sd_hispanic = sd(total_hispanic_enrollment, na.rm = TRUE),
    mean_white = mean(total_white_enrollment, na.rm = TRUE),
    sd_white = sd(total_white_enrollment, na.rm = TRUE)
  )
print(desc_stats_race)

anova_asian <- aov(total_asian_enrollment ~ borough, data = borough_clean)
summary(anova_asian)

anova_black <- aov(total_black_enrollment ~ borough, data = borough_clean)
summary(anova_black)

anova_hispanic <- aov(total_hispanic_enrollment ~ borough, data = borough_clean)
summary(anova_hispanic)

anova_white <- aov(total_white_enrollment ~ borough, data = borough_clean)
summary(anova_white)



perform_regression_economic <- function(data, race_column) {
  formula <- as.formula(paste(race_column, "~ economic_need_index"))
  model <- lm(formula, data = data)
  return(model)
}

borough_clean <- borough_clean %>%
  mutate(
    economic_need_index = as.numeric(gsub("%", "", economic_need_index)),
    poverty_1 = as.numeric(gsub("%", "", poverty_1)),
    total_asian_enrollment = grade_k * (asian_1 / 100) + grade_pk_half_day_full_day * (asian_1 / 100) + grade_3k * (asian_1 / 100),
    total_black_enrollment = grade_k * (black_1 / 100) + grade_pk_half_day_full_day * (black_1 / 100) + grade_3k * (black_1 / 100),
    total_hispanic_enrollment = grade_k * (hispanic_1 / 100) + grade_pk_half_day_full_day * (hispanic_1 / 100) + grade_3k * (hispanic_1 / 100),
    total_white_enrollment = grade_k * (white_1 / 100) + grade_pk_half_day_full_day * (white_1 / 100) + grade_3k * (white_1 / 100)
  )

perform_regression <- function(data, response, predictor) {
  model <- lm(as.formula(paste(response, "~", predictor)), data = data)
  summary(model)
}

regression_results_economic <- list(
  Asian = perform_regression(borough_clean, "total_asian_enrollment", "economic_need_index"),
  Black = perform_regression(borough_clean, "total_black_enrollment", "economic_need_index"),
  Hispanic = perform_regression(borough_clean, "total_hispanic_enrollment", "economic_need_index"),
  White = perform_regression(borough_clean, "total_white_enrollment", "economic_need_index")
)

regression_results_economic_df <- map_df(regression_results_economic, ~ data.frame(
  term = rownames(.$coefficients),
  estimate = .$coefficients[, "Estimate"],
  std_error = .$coefficients[, "Std. Error"],
  statistic = .$coefficients[, "t value"],
  p_value = .$coefficients[, "Pr(>|t|)"],
  race = names(regression_results_economic)[[1]]
), .id = "race")

regression_results_poverty <- list(
  Asian = perform_regression(borough_clean, "total_asian_enrollment", "poverty_1"),
  Black = perform_regression(borough_clean, "total_black_enrollment", "poverty_1"),
  Hispanic = perform_regression(borough_clean, "total_hispanic_enrollment", "poverty_1"),
  White = perform_regression(borough_clean, "total_white_enrollment", "poverty_1")
)

perform_regression_poverty <- function(data, race_column) {
  formula <- as.formula(paste(race_column, "~ poverty_1"))
  model <- lm(formula, data = data)
  return(model)
}

regression_results_poverty_df <- map_df(regression_results_poverty, ~ data.frame(
  term = rownames(.$coefficients),
  estimate = .$coefficients[, "Estimate"],
  std_error = .$coefficients[, "Std. Error"],
  statistic = .$coefficients[, "t value"],
  p_value = .$coefficients[, "Pr(>|t|)"],
  race = names(regression_results_poverty)[[1]]
), .id = "race")

plot_regression <- function(data, x_var, y_var, title, file_name) {
  ggplot(data, aes_string(x = x_var, y = y_var, color = "borough")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() +
    labs(title = title, x = x_var, y = y_var) +
    theme(legend.position = "bottom")
  ggsave(file_name, width = 10, height = 6)
}

plot_regression(borough_clean, "economic_need_index", "total_asian_enrollment", "Total Asian Enrollment vs Economic Need Index", "Total_Asian_Enrollment_vs_Economic_Need_Index.jpg")
plot_regression(borough_clean, "economic_need_index", "total_black_enrollment", "Total Black Enrollment vs Economic Need Index", "Total_Black_Enrollment_vs_Economic_Need_Index.jpg")
plot_regression(borough_clean, "economic_need_index", "total_hispanic_enrollment", "Total Hispanic Enrollment vs Economic Need Index", "Total_Hispanic_Enrollment_vs_Economic_Need_Index.jpg")
plot_regression(borough_clean, "economic_need_index", "total_white_enrollment", "Total White Enrollment vs Economic Need Index", "Total_White_Enrollment_vs_Economic_Need_Index.jpg")

plot_regression(borough_clean, "poverty_1", "total_asian_enrollment", "Total Asian Enrollment vs Poverty Rate", "Total_Asian_Enrollment_vs_Poverty_Rate.jpg")
plot_regression(borough_clean, "poverty_1", "total_black_enrollment", "Total Black Enrollment vs Poverty Rate", "Total_Black_Enrollment_vs_Poverty_Rate.jpg")
plot_regression(borough_clean, "poverty_1", "total_hispanic_enrollment", "Total Hispanic Enrollment vs Poverty Rate", "Total_Hispanic_Enrollment_vs_Poverty_Rate.jpg")
plot_regression(borough_clean, "poverty_1", "total_white_enrollment", "Total White Enrollment vs Poverty Rate", "Total_White_Enrollment_vs_Poverty_Rate.jpg")

plot_coefficients <- function(data, title, file_name) {
  ggplot(data, aes(x = term, y = estimate, fill = race)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = estimate - std_error, ymax = estimate + std_error), width = 0.2, position = position_dodge(0.9)) +
    theme_minimal() +
    labs(title = title, x = "Term", y = "Estimate") +
    scale_fill_manual(values = c("Asian" = "purple", "Black" = "violet", "Hispanic" = "orange", "White" = "skyblue")) +
    theme(legend.position = "bottom")
  ggsave(file_name, width = 10, height = 6)
}

plot_coefficients(regression_results_economic_df, "Regression Coefficients by Race (Economic Need Index)", "Regression_Coefficients_by_Race_Economic_Need_Index.jpg")

plot_coefficients(regression_results_poverty_df, "Regression Coefficients by Race (Poverty Rate)", "Regression_Coefficients_by_Race_Poverty_Rate.jpg")

regression_models_economic <- list(
  Asian = perform_regression_economic(borough_clean, "total_asian_enrollment"),
  Black = perform_regression_economic(borough_clean, "total_black_enrollment"),
  Hispanic = perform_regression_economic(borough_clean, "total_hispanic_enrollment"),
  White = perform_regression_economic(borough_clean, "total_white_enrollment")
)

regression_models_poverty <- list(
  Asian = perform_regression_poverty(borough_clean, "total_asian_enrollment"),
  Black = perform_regression_poverty(borough_clean, "total_black_enrollment"),
  Hispanic = perform_regression_poverty(borough_clean, "total_hispanic_enrollment"),
  White = perform_regression_poverty(borough_clean, "total_white_enrollment")
)

extract_coefs <- function(model_list, term) {
  map_dfr(model_list, ~data.frame(
    term = term,
    estimate = coef(.x)[term],
    std_error = sqrt(diag(vcov(.x)))[term]
  ), .id = "race")
}

coefs_economic <- extract_coefs(regression_models_economic, "economic_need_index")

coefs_poverty <- extract_coefs(regression_models_poverty, "poverty_1")

combined_coefs <- bind_rows(
  coefs_economic %>% mutate(term = "economic_need_index"),
  coefs_poverty %>% mutate(term = "poverty_1")
)

intercepts_economic <- extract_coefs(regression_models_economic, "(Intercept)")
intercepts_poverty <- extract_coefs(regression_models_poverty, "(Intercept)")

combined_intercepts <- bind_rows(
  intercepts_economic %>% mutate(term = "(Intercept)", race = paste(race, " (Economic Need Index)", sep = "")),
  intercepts_poverty %>% mutate(term = "(Intercept)", race = paste(race, " (Poverty Rate)", sep = ""))
)

ggplot(combined_coefs, aes(x = term, y = estimate, fill = race)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error),
                position = position_dodge(width = 0.9), width = 0.25) +
  theme_minimal() +
  labs(title = "Regression Coefficients by Race",
       x = "Term",
       y = "Estimate",
       fill = "Race") +
  theme(legend.position = "bottom")

ggsave("Regression_Coefficients_by_Race.jpg", width = 14, height = 8)

ggplot(combined_intercepts, aes(x = term, y = estimate, fill = race)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error),
                position = position_dodge(width = 0.9), width = 0.25) +
  theme_minimal() +
  labs(title = "Regression Intercepts by Race",
       x = "Term",
       y = "Estimate",
       fill = "Race") +
  theme(legend.position = "bottom")

ggsave("Regression_Intercepts_by_Race.jpg", width = 14, height = 8)

regression_summaries_economic <-lapply(regression_models_economic, summary)
regression_summaries_poverty <-lapply(regression_models_poverty, summary)

print("Economic Need Index Regression Results:")
print(regression_results_economic)

print("Poverty Rate Regression Results:")
print(regression_models_poverty)




race_trend <- borough_clean %>%
  group_by(year, borough) %>%
  summarize(
    total_asian_enrollment = mean(total_asian_enrollment, na.rm = TRUE),
    total_black_enrollment = mean(total_black_enrollment, na.rm = TRUE),
    total_hispanic_enrollment = mean(total_hispanic_enrollment, na.rm = TRUE),
    total_white_enrollment = mean(total_white_enrollment, na.rm = TRUE)
  ) %>%
  gather(key = "race", value = "enrollment", -year, -borough)

ggplot(race_trend, aes(x = year, y = enrollment, color = race, group = race)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ borough, scales = "fixed") +
  labs(title = "Enrollment Trends by Borough and Race", x = "Year", y = "Average Enrollment") +
  theme_minimal() +
  scale_color_manual(values = c("total_asian_enrollment" = "purple", "total_black_enrollment" = "violet", "total_hispanic_enrollment" = "orange", "total_white_enrollment" = "skyblue"))

ggsave("Enrollment Trends by Borough and Race.jpg", plot = last_plot(), width = 12, height = 8)

borough_clean <- borough_clean %>%
  mutate(
    asian_1 = parse_number(asian_1),
    black_1 = parse_number(black_1),
    hispanic_1 = parse_number(hispanic_1),
    white_1 = parse_number(white_1)
  )

race_data <- borough_clean %>%
  select(year, borough, grade_3k, grade_pk_half_day_full_day, grade_k,
         asian, asian_1, black, black_1, hispanic, hispanic_1, white, white_1) %>%
  filter(borough %in% c("Brooklyn", "Manhattan", "Bronx", "Queens", "Staten Island"))

race_data <- race_data %>%
  mutate(
    total_enrollment = grade_3k + grade_pk_half_day_full_day + grade_k,
    asian_enrollment = total_enrollment * asian_1 / 100,
    black_enrollment = total_enrollment * black_1 / 100,
    hispanic_enrollment = total_enrollment * hispanic_1 / 100,
    white_enrollment = total_enrollment * white_1 / 100
  )


long_data <- race_data %>%
  select(year, borough, asian_enrollment, black_enrollment, hispanic_enrollment, white_enrollment) %>%
  pivot_longer(
    cols = ends_with("enrollment"),
    names_to = "race",
    values_to = "enrollment",
    names_prefix = "",
    names_transform = list(race = ~ str_replace(., "_enrollment", ""))
  )

ggplot(long_data, aes(x = year, y = enrollment, color = borough, group = borough)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ race, scales = "fixed") +
  theme_minimal() +
  scale_color_manual(values = c("Brooklyn" = "orange", "Manhattan" = "red", "Bronx" = "blue", "Queens" = "skyblue", "Staten Island" = "violet" )) +
  labs(title = "Enrollment Trends in Early Education by Race and Borough",
       x = "Year",
       y = "Enrollment",
       color = "Borough") +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, NA))

ggsave("Enrollment_Trends_in_Early_Education_by_Race_and_Borough.jpg", plot = last_plot(), width = 10, height = 6)








    
