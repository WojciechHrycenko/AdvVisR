library(tidyverse)
library(scales)
library(ggalluvial)
library(treemapify)
library(ggcorrplot)

# Load data
df <- read_csv("Data/Final_data_model.csv")

# Preprocessing
df <- df %>%
  mutate(
    `Difficulty Level` = factor(`Difficulty Level`, levels = c("Beginner", "Intermediate", "Advanced")),
    Age_Group = cut(Age, breaks = c(0, 25, 35, 45, 100), labels = c("18-25", "26-35", "36-45", "45+")),
    Burn_Category = case_when(
      Calories_Burned < 500 ~ "Low Burn",
      Calories_Burned < 1000 ~ "Med Burn",
      TRUE ~ "High Burn"
    )
  )

# --- PLOT 1: SCALED SPIDER CHART ---
create_macro_radar <- function(data, group_col, plot_title) {
  custom_limits <- tibble(
    Nutrient = c("Proteins", "Fats", "Carbs"),
    Max_Limit = c(200, 100, 400)
  )
  
  radar_data <- data %>%
    group_by({{ group_col }}) %>%
    summarise(
      Proteins = mean(Proteins, na.rm = TRUE),
      Fats = mean(Fats, na.rm = TRUE),
      Carbs = mean(Carbs, na.rm = TRUE)
    ) %>%
    pivot_longer(-{{ group_col }}, names_to = "Nutrient", values_to = "Value") %>%
    left_join(custom_limits, by = "Nutrient") %>%
    mutate(Value_Norm = Value / Max_Limit)
  
  radar_coords <- radar_data %>%
    mutate(
      angle = case_when(
        Nutrient == "Proteins" ~ 90 * pi / 180,
        Nutrient == "Carbs"    ~ 330 * pi / 180,
        Nutrient == "Fats"     ~ 210 * pi / 180
      ),
      x = Value_Norm * cos(angle),
      y = Value_Norm * sin(angle)
    )
  
  grid_levels <- c(0.25, 0.50, 0.75, 1.00)
  grid_data <- expand_grid(level = grid_levels, angle = c(90, 330, 210) * pi / 180) %>%
    mutate(x = level * cos(angle), y = level * sin(angle)) %>%
    arrange(level, desc(angle)) %>%
    group_by(level) %>%
    slice(c(1:n(), 1))
  
  axis_labels <- expand_grid(Nutrient = c("Proteins", "Fats", "Carbs"), level = grid_levels) %>%
    left_join(custom_limits, by = "Nutrient") %>%
    mutate(
      Raw_Value = round(level * Max_Limit),
      angle = case_when(
        Nutrient == "Proteins" ~ 90 * pi / 180,
        Nutrient == "Carbs"    ~ 330 * pi / 180,
        Nutrient == "Fats"     ~ 210 * pi / 180
      ),
      x = level * cos(angle),
      y = level * sin(angle)
    )
  
  radar_closed <- radar_coords %>%
    arrange({{ group_col }}, desc(angle)) %>%
    group_by({{ group_col }}) %>%
    slice(c(1:n(), 1))
  
  ggplot() +
    geom_polygon(data = grid_data, aes(x, y, group = level), fill = NA, color = "grey85", linetype = "dashed") +
    geom_label(data = axis_labels, aes(x, y, label = Raw_Value), size = 2.5, color = "grey40", label.size = 0, fill = "white", alpha = 0.7) +
    geom_polygon(data = radar_closed, aes(x, y, color = {{ group_col }}, fill = {{ group_col }}), alpha = 0.2, linewidth = 1) +
    geom_point(data = radar_coords, aes(x, y, color = {{ group_col }}), size = 3) +
    annotate("text", x = 0, y = 1.15, label = "Proteins\n(Max 200g)", fontface = "bold") +
    annotate("text", x = 1.15 * cos(330*pi/180), y = 1.15 * sin(330*pi/180), label = "Carbs\n(Max 400g)", fontface = "bold", hjust = 0.5) +
    annotate("text", x = 1.15 * cos(210*pi/180), y = 1.15 * sin(210*pi/180), label = "Fats\n(Max 100g)", fontface = "bold", hjust = 0.5) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = plot_title)
}

print(create_macro_radar(df, `Difficulty Level`, "Macronutrient Profile by Difficulty (Scaled)"))
print(create_macro_radar(df, Workout_Type, "Macronutrient Profile by Workout Type (Scaled)"))
print(create_macro_radar(df, meal_type, "Macronutrient Profile by Meal Type (Scaled)"))

# --- PLOT 2: SCATTER PLOT (Training Efficiency) ---
p_scatter <- ggplot(df, aes(x = Avg_BPM, y = Calories_Burned, color = Workout_Type, size = `Session_Duration (hours)`)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  scale_size_continuous(range = c(1, 6), name = "Duration (hrs)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    title = "Training Efficiency Analysis",
    subtitle = "Impact of Intensity (BPM) on Calories Burned by Workout Type",
    x = "Average Heart Rate (BPM)",
    y = "Total Calories Burned"
  )
print(p_scatter)

# --- PLOT 3: TREEMAP ---
tree_data <- df %>%
  group_by(`Difficulty Level`, meal_type, cooking_method) %>%
  summarise(Total_Calories = sum(Calories, na.rm = TRUE), .groups = "drop") %>%
  filter(Total_Calories > 0) %>%
  mutate(meal_type = factor(meal_type, levels = c("Breakfast", "Lunch", "Dinner", "Snack")))

p_tree <- ggplot(tree_data, aes(area = Total_Calories, fill = meal_type, label = cooking_method, subgroup = meal_type)) +
  geom_treemap(layout = "squarified", color = "white", size = 2) +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(place = "topleft", grow = FALSE, colour = "#333333", fontface = "bold", size = 13, padding.x = grid::unit(3, "mm"), padding.y = grid::unit(3, "mm"), alpha = 1) +
  geom_treemap_text(aes(label = paste0(cooking_method, "\n", format(round(Total_Calories, 0), big.mark = " ", trim = TRUE))), colour = "white", place = "centre", grow = FALSE, reflow = TRUE) +
  facet_wrap(~ `Difficulty Level`) +
  scale_fill_brewer(palette = "Set2") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(
    title = "Caloric Share by Meal Type & Preparation",
    subtitle = "Size represents Total Calories | Ordered: Breakfast → Lunch → Dinner → Snack",
    fill = "Meal Type"
  )
print(p_tree)

# --- PLOT 4: ALLUVIAL PLOT ---
alluvial_data <- df %>%
  mutate(
    Experience_Cat = case_when(
      Experience_Level <= 1.5 ~ "Beginner",
      Experience_Level <= 2.5 ~ "Intermediate",
      TRUE ~ "Advanced"
    ),
    Experience_Cat = factor(Experience_Cat, levels = c("Beginner", "Intermediate", "Advanced")),
    # Using created Burn_Category instead of potential csv column Burns_Calories_Bin for consistency
    Burn_Category = factor(Burn_Category, levels = c("Low Burn", "Med Burn", "High Burn")) 
  ) %>%
  count(Experience_Cat, Workout_Type, Burn_Category)

p_alluvial <- ggplot(alluvial_data, aes(axis1 = Experience_Cat, axis2 = Workout_Type, axis3 = Burn_Category, y = n)) +
  geom_alluvium(aes(fill = Experience_Cat), width = 1/12, alpha = 0.7, knot.pos = 0.4) +
  geom_stratum(width = 1/12, fill = "grey95", color = "grey20") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, fontface = "bold") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_x_discrete(limits = c("Experience", "Workout Type", "Burn Intensity"), expand = c(.05, .05)) +
  labs(
    title = "Workout Flows: Experience to Intensity",
    subtitle = "Tracing the path from experience level to calorie burn efficiency",
    fill = "Experience Level",
    y = "Number of Sessions"
  )
print(p_alluvial)

# --- PLOT 5: BMI HISTOGRAM ---
p_bmi <- ggplot(df, aes(x = BMI, fill = Gender)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "identity", color = "white") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(face = "bold", size = 14)) +
  labs(title = "BMI Distribution by Gender", x = "BMI Value", y = "Number of People")
print(p_bmi)

# --- PLOT 6: BAR CHART (TOP EXERCISES) ---
top_exercises <- df %>%
  count(Name_of_Exercise, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(Name_of_Exercise)

bar_data <- df %>%
  filter(Name_of_Exercise %in% top_exercises) %>%
  mutate(Freq_Day = round(`Workout_Frequency (days/week)`, 0))

p_freq_exercise <- ggplot(bar_data, aes(x = factor(Freq_Day), fill = Name_of_Exercise)) +
  geom_bar(color = "black", alpha = 0.8, width = 0.7, show.legend = FALSE) +
  facet_wrap(~ Name_of_Exercise, scales = "free_y") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Workout Frequency by Exercise Name (Top 6)",
    subtitle = "Distribution of weekly training days for most popular exercises",
    x = "Days per Week",
    y = "Number of People"
  )
print(p_freq_exercise)

# --- PLOT 7: CORRELATION PLOT ---
corr_matrix <- df %>%
  select(Calories, Proteins, Fats, Carbs) %>%
  cor(use = "complete.obs", method = "pearson")

p_corr <- ggcorrplot(corr_matrix, method = "circle", type = "lower", lab = TRUE, lab_size = 5,
                     colors = c("#E46726", "white", "#6D9EC1"),
                     title = "Correlation Matrix: Calories vs. Macronutrients",
                     ggtheme = theme_minimal()) +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "right")
print(p_corr)

# --- PLOT 8: GROUPED BAR CHART ---
grouped_data <- df %>%
  mutate(Age_Group = cut(Age, breaks = c(-Inf, 30, 45, 60, Inf), labels = c("18-30", "31-45", "46-60", "60+"))) %>%
  filter(!is.na(Age_Group)) %>%
  group_by(Age_Group, Gender, Workout_Type) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  mutate(Percentage = Count / sum(Count) * 100)

p_grouped <- ggplot(grouped_data, aes(x = Age_Group, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black", alpha = 0.8) +
  facet_wrap(~ Workout_Type) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  labs(
    title = "Workout Preference by Age & Gender",
    subtitle = "Percentage of workout types chosen within each demographic group",
    x = "Age Group",
    y = "Percentage share",
    fill = "Gender"
  )
print(p_grouped)