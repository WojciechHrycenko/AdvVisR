library(tidyverse)
library(scales)
library(ggalluvial)
library(treemapify)
library(ggcorrplot)
library(ggridges)
library(ggiraph)

#setwd("C:/Users/wojci/Desktop/VisR/AdvVisR/")

# --- 1. DATA LOAD & PREPROCESSING ---
df <- read_csv("Data/Final_data_model.csv")

# Clean names & Create Factors
df <- df %>%
  mutate(
    # Simplify column names
    Water_L = `Water_Intake (liters)`,
    Duration_Hrs = `Session_Duration (hours)`,
    
    # Set logical order for factors
    `Difficulty Level` = factor(`Difficulty Level`, levels = c("Beginner", "Intermediate", "Advanced")),
    meal_type = factor(meal_type, levels = c("Breakfast", "Lunch", "Dinner", "Snack")),
    
    # Create Caloric Burn Categories for Alluvial Plot
    Burn_Category = case_when(
      Calories_Burned < 400 ~ "Low Burn",
      Calories_Burned < 800 ~ "Med Burn",
      TRUE ~ "High Burn"
    ),
    Burn_Category = factor(Burn_Category, levels = c("Low Burn", "Med Burn", "High Burn"))
  )


# 1. Definiujemy rodzinę czcionek dla silnika Quartz (Mac)
quartzFonts(Times = c("Times New Roman", "Times New Roman Bold", "Times New Roman Italic", "Times New Roman Bold Italic"))

# 2. Ustawiamy domyślny motyw z tą czcionką
theme_set(theme_minimal(base_family = "Times"))

# 3. Aktualizujemy domyślne ustawienia dla tekstów WEWNĄTRZ wykresów
update_geom_defaults("text", list(family = "Times"))
update_geom_defaults("label", list(family = "Times"))

# ==============================================================================
# PART 1: INPUT (THE FUEL)
# Analyzing Nutrition, Diet Types, and Macro Profiles
# ==============================================================================

# --- PLOT 1: TREEMAP (The Plate Under Microscope) ---
# Goal: Visualize Caloric Volume vs. Sugar Content across Diets and Meals.
tree_data <- df %>%
  group_by(diet_type, meal_type) %>%
  summarise(Total_Calories = sum(Calories, na.rm = TRUE), 
            Avg_Sugar = mean(sugar_g, na.rm = TRUE), .groups = "drop")

p_tree <- ggplot(tree_data, aes(area = Total_Calories, fill = Avg_Sugar, subgroup = diet_type, label = meal_type)) +
  geom_treemap(color = "white") +
  # Diet Label (Watermark style)
  geom_treemap_subgroup_border(color = "white", size = 3) +
  geom_treemap_subgroup_text(place = "topleft", grow = F, alpha = 0.5, colour = "black", fontface = "italic") +
  # Meal Label
  geom_treemap_text(colour = "white", place = "centre", grow = FALSE) +
  scale_fill_gradient(low = "#69b3a2", high = "#c0392b", name = "Avg Sugar (g)") +
  labs(title = "Plate Under Microscope", subtitle = "Caloric Volume vs. Sugar Content by Diet") +
  theme(legend.position = "bottom")

print(p_tree)


# --- PLOT 2: RADAR CHART (Macro Profile) ---
# Goal: Compare nutritional balance (normalized 0-1) across diets using small multiples.
create_faceted_radar <- function(data) {
  
  # 1. Select & Normalize Data (Scale relative to max observed value)
  radar_data <- data %>%
    group_by(diet_type) %>%
    summarise(
      Proteins = mean(Proteins, na.rm = TRUE), 
      Fats = mean(Fats, na.rm = TRUE), 
      Carbs = mean(Carbs, na.rm = TRUE),
      Water = mean(Water_L, na.rm = TRUE), 
      Sodium = mean(sodium_mg, na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), ~ .x / max(.x))) %>% 
    pivot_longer(-diet_type, names_to = "Metric", values_to = "Value")
  
  # 2. Geometry Setup (Order metrics for correct polygon drawing)
  metric_order <- c("Proteins", "Fats", "Sodium", "Water", "Carbs")
  
  radar_coords <- radar_data %>%
    mutate(Metric = factor(Metric, levels = metric_order)) %>% 
    arrange(diet_type, Metric) %>% 
    mutate(
      angle_idx = as.numeric(Metric),
      angle = 2 * pi * (angle_idx - 1) / length(unique(Metric)),
      x = Value * sin(angle),
      y = Value * cos(angle)
    )
  
  # 3. Label Coordinates
  labels_coords <- tibble(
    Metric = metric_order,
    angle = 2 * pi * (0:(length(metric_order)-1)) / length(metric_order)
  ) %>%
    mutate(x = 1.3 * sin(angle), y = 1.3 * cos(angle))
  
  # 4. Plot
  ggplot() +
    # Background Grid
    geom_polygon(data = expand_grid(level = c(0.25, 0.5, 0.75, 1.0), 
                                    angle = seq(0, 2*pi, length.out = 50)), 
                 aes(x = level * sin(angle), y = level * cos(angle), group = level), 
                 fill = NA, color = "grey90") +
    # Data Polygons
    geom_polygon(data = radar_coords, aes(x = x, y = y, group = diet_type), 
                 fill = "#404080", alpha = 0.5, color = "#404080", linewidth = 1) +
    # Points & Labels
    geom_point(data = radar_coords, aes(x = x, y = y), color = "white", size = 2) +
    geom_point(data = radar_coords, aes(x = x, y = y), color = "#404080", size = 1) +
    geom_text(data = labels_coords, aes(x = x, y = y, label = Metric), 
              size = 2.5, fontface = "bold", color = "grey30") +
    facet_wrap(~ diet_type) +
    coord_fixed() + theme_void() +
    labs(title = "Nutritional Radar by Diet", subtitle = "Scale: Center=0, Edge=Max observed value")
}

print(create_faceted_radar(df))


# ==============================================================================
# PART 2: PROCESS (THE MACHINE)
# Analyzing Decisions, Engine Performance, and Targeted Anatomy
# ==============================================================================

# --- PLOT 3: ALLUVIAL PLOT (The Warrior's Path) ---
# Goal: Trace the user flow: Experience -> Workout Choice -> Burn Efficiency.
p_alluvial <- df %>%
  count(`Difficulty Level`, Workout_Type, Burn_Category) %>%
  ggplot(aes(axis1 = `Difficulty Level`, axis2 = Workout_Type, axis3 = Burn_Category, y = n)) +
  geom_alluvium(aes(fill = `Difficulty Level`), width = 1/8, alpha = 0.7, curve_type = "cubic") +
  geom_stratum(width = 1/8, fill = "grey95", color = "grey40", size = 0.3) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.5, fontface = "bold") +
  scale_x_discrete(limits = c("Experience", "Workout", "Burn Intensity"), expand = c(.05, .05)) +
  scale_fill_brewer(palette = "Dark2", name = "Experience Group") + 
  theme_void() +
  labs(title = "The Warrior's Path", subtitle = "Tracing the flow: Experience Level → Workout Choice → Caloric Efficiency") +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16), plot.margin = margin(10, 10, 10, 10))

print(p_alluvial)


# --- PLOT 4: INTERACTIVE SCATTER (Engine Performance) ---
# Goal: Interactive view of Burn vs Intensity vs Duration.
p_interactive <- ggplot(df, aes(x = Avg_BPM, y = Calories_Burned, size = Duration_Hrs, color = Workout_Type)) +
  geom_point_interactive(
    aes(tooltip = Name_of_Exercise, data_id = Name_of_Exercise), 
    alpha = 0.4, stroke = 0
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_size_continuous(range = c(0.5, 3.0), name = "Duration (h)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "right") +
  labs(title = "Engine Performance (Interactive)", subtitle = "Hover points to see Exercise Name", 
       x = "Average Heart Rate (BPM)", y = "Total Calories Burned")

# Render with fixed aspect ratio for Viewer
print(girafe(
  ggobj = p_interactive,
  width_svg = 10, height_svg = 5,  
  options = list(opts_sizing(rescale = TRUE, width = 1.0), opts_hover(css = "fill:black;stroke:black;r:4pt;"))
))


# --- PLOT 5: CIRCULAR BARPLOT (Anatomical Tachometer) ---
# Goal: Visualize targeted muscle groups as a technical gauge / blueprint.
muscle_data <- df %>%
  count(`Target Muscle Group`, sort = TRUE) %>%
  mutate(
    Highlight = ifelse(n > mean(n), "Major Focus", "Minor Focus"),
    Label_Text = paste0(`Target Muscle Group`, " (", n, ")")
  )

max_val <- max(muscle_data$n)
label_pos <- -max_val * 0.02 
limit_negative <- -max_val * 0.6 

p_muscle <- ggplot(muscle_data, aes(x = reorder(`Target Muscle Group`, n), y = n)) +
  # Background Tracks
  geom_col(aes(y = max_val), fill = "#f0f0f0", width = 0.6) +
  # Data Bars
  geom_col(aes(fill = Highlight), width = 0.6) +
  # Labels (Right of start line)
  geom_text(aes(y = label_pos, label = Label_Text), hjust = 1, color = "#2c3e50", fontface = "bold", size = 3.2) +
  coord_polar(theta = "y", start = pi, direction = 1) +
  scale_fill_manual(
    name = "Training Volume",
    values = c("Major Focus" = "#E46726", "Minor Focus" = "#6D9EC1"),
    labels = c("Major Focus (> Avg)", "Minor Focus (< Avg)")
  ) +
  scale_y_continuous(limits = c(limit_negative, max_val)) +
  scale_x_discrete(expand = expansion(add = c(0.4, 0.5))) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey50", margin = margin(b = -10)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9, color = "grey30"),
    plot.margin = margin(0, 20, 0, 0)
  ) +
  labs(title = "Anatomical Tachometer", subtitle = "Muscle Activation Load (RPM)")

print(p_muscle)


# ==============================================================================
# PART 3: OUTPUT (THE RESULTS)
# Analyzing Physiological Adaptation and Body Composition
# ==============================================================================

# --- PLOT 6: ENHANCED DUMBBELL (Heart Rate Reserve) ---
# Goal: Show heart usage (Resting -> Max) and actual Training Intensity.
dumbbell_data <- df %>%
  group_by(`Difficulty Level`) %>%
  summarise(
    Resting = mean(Resting_BPM, na.rm = TRUE),
    Max = mean(Max_BPM, na.rm = TRUE),
    Avg_Workout = mean(Avg_BPM, na.rm = TRUE)
  )

p_dumbbell <- ggplot(dumbbell_data, aes(y = `Difficulty Level`)) +
  # Reserve Bar (Grey)
  geom_segment(aes(x = Resting, xend = Max, yend = `Difficulty Level`), color = "grey90", linewidth = 6, lineend = "round") +
  # Usage Bar (Green)
  geom_segment(aes(x = Resting, xend = Avg_Workout, yend = `Difficulty Level`), color = "#69b3a2", linewidth = 6, lineend = "round") +
  # Points
  geom_point(aes(x = Resting), color = "#2c3e50", size = 5) +
  geom_point(aes(x = Max), color = "#e74c3c", size = 5) +
  geom_point(aes(x = Avg_Workout), color = "white", size = 3, shape = 18) +
  # Labels
  geom_text(aes(x = Resting, label = paste0(round(Resting), " bpm")), vjust = 2.5, size = 3, fontface = "bold", color = "#2c3e50") +
  geom_text(aes(x = Max, label = paste0(round(Max), " bpm")), vjust = 2.5, size = 3, fontface = "bold", color = "#e74c3c") +
  geom_text(aes(x = Avg_Workout, label = "Train Avg"), vjust = -1.8, size = 2.5, fontface = "italic", color = "#69b3a2") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16), panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold", size = 11)) +
  labs(title = "Heart Rate Reserve Analysis", subtitle = "Grey Bar: Total Reserve | Green Bar: Actual Training Intensity Used", x = "Heart Rate (BPM)", y = "")

print(p_dumbbell)


# --- PLOT 7: RIDGELINE (Mountains of Fat) ---
# Goal: Visualize body fat distribution by Gender and Experience Level.
ridge_data <- df %>%
  mutate(
    Group_Label = paste(Gender, "|", `Difficulty Level`),
    Group_Label = factor(Group_Label, levels = c(
      "Male | Advanced", "Male | Intermediate", "Male | Beginner",
      "Female | Advanced", "Female | Intermediate", "Female | Beginner"
    ))
  ) %>%
  filter(!is.na(Group_Label))

p_ridge <- ggplot(ridge_data, aes(x = Fat_Percentage, y = Group_Label, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2, size = 0.3, color = "white") +
  scale_fill_viridis_c(name = "Body Fat %", option = "C") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16), axis.text.y = element_text(face = "bold", size = 10), panel.grid.major.y = element_blank()) +
  labs(title = "Mountains of Fat", subtitle = "Distribution of Body Fat % by Gender & Experience (White line = Median)", x = "Body Fat Percentage", y = "")

print(p_ridge)


# --- PLOT 8: CORRELATION MATRIX (Data Science Summary) ---
# Goal: Statistical validation of key metric relationships.
corr_data <- df %>%
  select(Age, `Weight (kg)`, BMI, Calories_Burned, Avg_BPM, Duration_Hrs) %>%
  rename(Weight = `Weight (kg)`, Duration = Duration_Hrs) %>%
  cor(use = "complete.obs")

p_corr <- ggcorrplot(corr_data, method = "circle", type = "lower", lab = TRUE, lab_size = 3, 
                     colors = c("#E46726", "white", "#6D9EC1"), title = "Key Metrics Correlations")
print(p_corr)