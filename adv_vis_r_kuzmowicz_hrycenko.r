library(tidyverse)
library(scales)
library(ggalluvial)
library(treemapify)
library(ggcorrplot)
library(ggridges)
library(ggiraph)
library(showtext)

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
# --- STANDARD COLORS & THEME ---
col_teal <- "#69b3a2"  # The "Green/Teal" (Cool / Good / Low)
col_red  <- "#e74c3c"  # The "Red" (Hot / Intense / High)
col_grey <- "grey90"
font_main <- "Geologica"

# Unified Theme Function
theme_unified <- function() {
  theme_minimal(base_family = font_main) +
    theme(
      # Titles & Subtitles
      plot.title = element_text(family = font_main, face = "bold", size = 22, hjust = 0.5, color = "black"),
      plot.subtitle = element_text(family = font_main, size = 14, hjust = 0.5, color = "black", margin = margin(b = 20)),
      
      # Text Elements
      text = element_text(family = font_main, color = "black"),
      axis.text = element_text(family = font_main, color = "black", size = 10),
      axis.title = element_text(family = font_main, face = "bold", color = "black"),
      strip.text = element_text(family = font_main, face = "bold", size = 12, color = "black"),
      
      # Legends
      legend.title = element_text(family = font_main, face = "bold", color = "black"),
      legend.text = element_text(family = font_main, color = "black"),
      
      # Backgrounds
      panel.grid.minor = element_blank()
    )
}

# ==============================================================================
# PART 1: INPUT (THE FUEL)
# Analyzing Nutrition, Diet Types, and Macro Profiles
# ==============================================================================

# --- PLOT 1: TREEMAP (The Plate Under Microscope) ---
# Goal: Visualize Caloric Volume vs. Sugar Content across Diets and Meals.
# Approach: Small Multiples (Faceting) with Enriched Title
tree_data <- df %>%
  group_by(diet_type, meal_type) %>%
  summarise(Total_Calories = sum(Calories, na.rm = TRUE), 
            Avg_Sugar = mean(sugar_g, na.rm = TRUE), .groups = "drop")

p_tree <- ggplot(tree_data, aes(area = Total_Calories, fill = Avg_Sugar, label = meal_type)) +
  geom_treemap(color = "white", size = 2) +
  geom_treemap_text(colour = "white", place = "centre", grow = FALSE, reflow = TRUE, family = font_main) +
  facet_wrap(~ diet_type) +
  
  # UNIFIED COLORS
  scale_fill_gradient(low = col_teal, high = col_red, name = "Avg Sugar (g)") +
  
  labs(title = "Plate Under Microscope", 
       subtitle = "Caloric Volume vs. Sugar Content by Diet") +
  
  theme_unified() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

print(p_tree)


# ==============================================================================
# PLOT 2: RADAR CHART (Fixed Label Overlap)
# ==============================================================================

create_faceted_radar <- function(data) {
  # (Data prep same as before...)
  summary_raw <- data %>% group_by(diet_type) %>%
    summarise(Proteins = mean(Proteins, na.rm=T), Fats = mean(Fats, na.rm=T), Carbs = mean(Carbs, na.rm=T)) %>%
    ungroup() %>% pivot_longer(-diet_type, names_to = "Metric", values_to = "Value_Raw")
  
  summary_norm <- data %>% group_by(diet_type) %>%
    summarise(Proteins = mean(Proteins, na.rm=T), Fats = mean(Fats, na.rm=T), Carbs = mean(Carbs, na.rm=T)) %>%
    mutate(across(where(is.numeric), ~ .x / max(.x, na.rm = TRUE))) %>% ungroup() %>%
    pivot_longer(-diet_type, names_to = "Metric", values_to = "Value_Norm")
  
  radar_data <- left_join(summary_norm, summary_raw, by = c("diet_type", "Metric"))
  metric_order <- c("Proteins", "Fats", "Carbs")
  
  radar_coords <- radar_data %>%
    mutate(Metric = factor(Metric, levels = metric_order)) %>% arrange(diet_type, Metric) %>% 
    mutate(angle_idx = as.numeric(Metric), angle = 2*pi*(angle_idx-1)/length(unique(Metric)),
           x = Value_Norm * sin(angle), y = Value_Norm * cos(angle))
  
  labels_coords <- tibble(Metric = metric_order, angle = 2*pi*(0:2)/3) %>%
    mutate(x = 1.6 * sin(angle), y = 1.6 * cos(angle))
  
  ggplot() +
    geom_polygon(data = expand_grid(level = c(0.25, 0.5, 0.75, 1.0), angle = seq(0, 2*pi, length.out = 4)), 
                 aes(x = level*sin(angle), y = level*cos(angle), group = level), fill = NA, color = "grey90", size = 0.3) +
    geom_polygon(data = radar_coords, aes(x = x, y = y, group = diet_type, fill = diet_type, color = diet_type), alpha = 0.4, linewidth = 1) +
    geom_point(data = radar_coords, aes(x = x, y = y, color = diet_type), size = 3) +
    
    # UNIFIED FONT
    geom_text(data = labels_coords, aes(x = x, y = y, label = Metric), size = 4, color = "black", family = font_main) +
    
    facet_wrap(~ diet_type) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    coord_fixed(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), clip = "off") + 
    
    theme_void(base_family = font_main) +
    labs(title = "Nutritional Radar by Diet", subtitle = "Shape: Normalized relative to max") +
    
    # UNIFIED THEME OVERRIDES
    theme(
      plot.title = element_text(family = font_main, face = "bold", size = 22, hjust = 0.5, color = "black"),
      plot.subtitle = element_text(family = font_main, size = 14, hjust = 0.5, margin = margin(b = 20), color = "black"),
      strip.text = element_text(family = font_main, face = "bold", size = 14, margin = margin(b = 15), color = "black"),
      legend.position = "none"
    )
}
print(create_faceted_radar(df))


# ==============================================================================
# PART 2: PROCESS (THE MACHINE)
# Analyzing Decisions, Engine Performance, and Targeted Anatomy
# ==============================================================================

# --- PLOT 3: ALLUVIAL PLOT (The Warrior's Path) ---

p_alluvial <- df %>%
  count(`Difficulty Level`, Workout_Type, Burn_Category) %>%
  ggplot(aes(axis1 = `Difficulty Level`, axis2 = Workout_Type, axis3 = Burn_Category, y = n)) +
  geom_alluvium(aes(fill = `Difficulty Level`), width = 1/8, alpha = 0.7, curve_type = "cubic") +
  geom_stratum(width = 1/8, fill = "grey95", color = "grey40", size = 0.3) +
  
  # UNIFIED FONT
  geom_text(stat = "stratum", aes(label = paste0(after_stat(stratum), "\n", scales::percent(after_stat(prop), accuracy = 1))), 
            size = 3, fontface = "bold", lineheight = 0.8, family = font_main, color = "black") +
  
  scale_x_discrete(limits = c("Experience", "Workout", "Burn Intensity"), expand = c(.05, .05)) +
  scale_fill_brewer(palette = "Dark2", name = "Experience Group") + 
  
  theme_void(base_family = font_main) +
  labs(title = "The Warrior's Path", subtitle = "Tracking the flow: Experience Level → Workout Choice → Caloric Efficiency") +
  
  # UNIFIED THEME OVERRIDES
  theme(
    plot.title = element_text(family = font_main, face = "bold", size = 22, hjust = 0.5, color = "black"),
    plot.subtitle = element_text(family = font_main, size = 14, hjust = 0.5, color = "black", margin = margin(b = 10)),
    legend.position = "bottom",
    legend.text = element_text(family = font_main, color = "black"),
    legend.title = element_text(family = font_main, face = "bold", color = "black"),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p_alluvial)


## --- PLOT 4: ENGINE HEATMAP (Static Replacement) ---
# Goal: Visualize the relationship between Intensity (BPM) and Output (Calories) 
# using density contours to handle large data volume.

p_static <- ggplot(df, aes(x = Avg_BPM, y = Calories_Burned)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", color = "white", size = 0.1) +
  facet_wrap(~ Workout_Type, ncol = 2) +
  
  scale_fill_viridis_c(
    option = "inferno", name = "Concentration",
    guide = guide_colorbar(title.position = "top", label.theme = element_text(size = 8, face = "bold", family = font_main, color = "black"), barheight = unit(5, "lines")),
    labels = NULL 
  ) +
  
  labs(title = "Engine Combustion Zones", 
       subtitle = "Intensity (Heart Rate) vs. Output (Calories) Distribution",
       x = "Average Heart Rate (BPM)", y = "Calories Burned") +
  
  theme_unified() +
  theme(legend.position = "right")

print(p_static)



# ==============================================================================
# PART 3: OUTPUT (THE RESULTS)
# Analyzing Physiological Adaptation and Body Composition
# ==============================================================================

# --- PLOT 6: ENHANCED DUMBBELL (Heart Rate Reserve) ---

dumbbell_data <- df %>%
  group_by(`Difficulty Level`) %>%
  summarise(
    Resting = mean(Resting_BPM, na.rm = TRUE),
    Max = mean(Max_BPM, na.rm = TRUE),
    Avg_Workout = mean(Avg_BPM, na.rm = TRUE)
  )

p_dumbbell <- ggplot(dumbbell_data, aes(y = `Difficulty Level`)) +
  geom_segment(aes(x = Resting, xend = Max, yend = `Difficulty Level`), color = col_grey, linewidth = 6, lineend = "round") +
  geom_segment(aes(x = Resting, xend = Avg_Workout, yend = `Difficulty Level`), color = col_teal, linewidth = 6, lineend = "round") +
  
  geom_point(aes(x = Resting), color = "#2c3e50", size = 5) +
  geom_point(aes(x = Max), color = col_red, size = 5) +
  geom_point(aes(x = Avg_Workout), color = "white", size = 3) +
  
  # UNIFIED FONTS
  geom_text(aes(x = Resting, label = paste0(round(Resting), " bpm")), vjust = 2.5, size = 3, fontface = "bold", color = "black", family = font_main) +
  geom_text(aes(x = Max, label = paste0(round(Max), " bpm")), vjust = 2.5, size = 3, fontface = "bold", color = col_red, family = font_main) +
  geom_text(aes(x = Avg_Workout, label = paste0("Train Avg: ", round(Avg_Workout), " bpm")), vjust = -1.8, size = 3, fontface = "bold", color = col_teal, family = font_main) +
  
  scale_y_discrete(limits = rev) +
  
  labs(title = "Heart Rate Reserve Analysis", 
       subtitle = "Green Bar: Actual Training Intensity Used | Grey Bar: Left Reserve", 
       x = "Heart Rate (BPM)", y = "") +
  
  theme_unified() +
  theme(panel.grid.major.y = element_blank())

print(p_dumbbell)


# --- PLOT 7: RIDGELINE (Mountains of Fat) ---

ridge_data <- df %>%
  mutate(
    Group_Label = paste(Gender, "|", `Difficulty Level`),
    Group_Label = factor(Group_Label, levels = c("Male | Advanced", "Female | Advanced", "Male | Intermediate", "Female | Intermediate", "Male | Beginner", "Female | Beginner"))
  ) %>% filter(!is.na(Group_Label))

p_ridge <- ggplot(ridge_data, aes(x = Fat_Percentage, y = Group_Label, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2, size = 0.3, color = "white") +
  scale_fill_viridis_c(name = "Body Fat %", option = "C") +
  
  labs(title = "Mountains of Fat", 
       subtitle = "Height = Density (Relative num. of people)\nWhite line = Median", 
       x = "Body Fat Percentage [%]", y = "") +
  
  theme_unified() +
  theme(panel.grid.major.y = element_blank())

print(p_ridge)


# --- PLOT 8: CORRELATION MATRIX (Data Science Summary) ---
# Goal: Statistical validation of key metric relationships.
corr_data <- df %>%
  select(Age, `Weight (kg)`, BMI, Calories_Burned, Avg_BPM, Duration_Hrs, Resting_BPM, `Workout_Frequency (days/week)`) %>%
  rename(Weight = `Weight (kg)`, Duration = Duration_Hrs, `Resting HR` = Resting_BPM, Frequency = `Workout_Frequency (days/week)`) %>%
  cor(use = "complete.obs")

p_corr <- ggcorrplot(
  corr_data, method = "circle", type = "lower", hc.order = TRUE, lab = TRUE, lab_size = 3.5, outline.color = "white",
  colors = c(col_teal, "white", col_red),
  title = "Correlation Matrix",
  ggtheme = theme_unified() # Applies our unified font/title settings directly
) +
  theme(panel.grid = element_blank())

print(p_corr)