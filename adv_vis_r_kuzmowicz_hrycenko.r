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

tryCatch(
  {
    font_add_google("Geologica", "Geologica", db_cache = FALSE)
  },
  error = function(e) {
    # Jeśli Geologica nadal nie działa, załadujemy bezpieczną alternatywę (Roboto)
    message("Geologica not found, loading Roboto instead.")
    font_add_google("Roboto", "Geologica") 
  }
)

showtext_auto()

# Ustawienie domyślne
theme_set(theme_minimal(base_family = "Geologica"))
update_geom_defaults("text", list(family = "Geologica"))
update_geom_defaults("label", list(family = "Geologica"))

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
  # Main Treemap squares
  geom_treemap(color = "white", size = 2) +
  
  # Meal Labels
  geom_treemap_text(colour = "white", place = "centre", grow = FALSE, reflow = TRUE) +
  
  # 6 Small Plots (Faceting by Diet)
  facet_wrap(~ diet_type) +
  
  # Colors
  scale_fill_gradient(low = "#69b3a2", high = "#c0392b", name = "Avg Sugar (g)") +
  
  # Titles
  labs(title = "Plate Under Microscope", 
       subtitle = "Caloric Volume vs. Sugar Content by Diet") +
  
  # Theme adjustments
  theme_minimal(base_family = "Times") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    
    # --- MODIFICATIONS HERE ---
    plot.title = element_text(face = "bold", size = 22), # Bigger & Bold
    strip.text = element_text(face = "bold", size = 14)  # Diet names
  )

print(p_tree)


# ==============================================================================
# PLOT 2: RADAR CHART (Fixed Label Overlap)
# ==============================================================================

create_faceted_radar <- function(data) {
  
  # --- 1. DATA PREPARATION ---
  
  # A. Calculate Raw Averages (Keep for calculation, even if not displayed)
  summary_raw <- data %>%
    group_by(diet_type) %>%
    summarise(
      Proteins = mean(Proteins, na.rm = TRUE), 
      Fats = mean(Fats, na.rm = TRUE), 
      Carbs = mean(Carbs, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    pivot_longer(-diet_type, names_to = "Metric", values_to = "Value_Raw")
  
  # B. Calculate Normalized Values (for Shape Geometry)
  summary_norm <- data %>%
    group_by(diet_type) %>%
    summarise(
      Proteins = mean(Proteins, na.rm = TRUE), 
      Fats = mean(Fats, na.rm = TRUE), 
      Carbs = mean(Carbs, na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), ~ .x / max(.x, na.rm = TRUE))) %>% 
    ungroup() %>%
    pivot_longer(-diet_type, names_to = "Metric", values_to = "Value_Norm")
  
  # C. Join Data
  radar_data <- left_join(summary_norm, summary_raw, by = c("diet_type", "Metric"))
  
  
  # --- 2. GEOMETRY SETUP ---
  
  metric_order <- c("Proteins", "Fats", "Carbs")
  
  radar_coords <- radar_data %>%
    mutate(Metric = factor(Metric, levels = metric_order)) %>% 
    arrange(diet_type, Metric) %>% 
    mutate(
      angle_idx = as.numeric(Metric),
      angle = 2 * pi * (angle_idx - 1) / length(unique(Metric)),
      
      # Plotting coordinates (Shape)
      x = Value_Norm * sin(angle),
      y = Value_Norm * cos(angle)
    )
  
  # Axis Labels Coordinates (The words "Proteins", "Fats", etc.)
  # Positioned at 1.6 distance
  labels_coords <- tibble(
    Metric = metric_order,
    angle = 2 * pi * (0:(length(metric_order)-1)) / length(metric_order)
  ) %>%
    mutate(x = 1.6 * sin(angle), y = 1.6 * cos(angle))
  
  
  # --- 3. PLOT ---
  
  ggplot() +
    # Background Grid (Triangle)
    geom_polygon(data = expand_grid(level = c(0.25, 0.5, 0.75, 1.0), 
                                    angle = seq(0, 2*pi, length.out = 4)), 
                 aes(x = level * sin(angle), y = level * cos(angle), group = level), 
                 fill = NA, color = "grey90", size = 0.3) +
    
    # Radar Shapes
    geom_polygon(data = radar_coords, aes(x = x, y = y, group = diet_type, fill = diet_type, color = diet_type), 
                 alpha = 0.4, linewidth = 1) +
    
    # Points on Vertices
    geom_point(data = radar_coords, aes(x = x, y = y, color = diet_type), size = 3) +
    
    # Axis Labels (Proteins, Fats...)
    geom_text(data = labels_coords, aes(x = x, y = y, label = Metric), 
              size = 4, color = "grey30", family = "Geologica") +
    
    # Faceting
    facet_wrap(~ diet_type) +
    
    # Styling
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    
    # ZOOMED IN: Limits reduced from 1.9 to 1.5 to make chart bigger
    coord_fixed(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), clip = "off") + 
    
    theme_void(base_family = "Geologica") +
    
    labs(title = "Nutritional Radar by Diet", 
         subtitle = "Shape: Normalized relative to max") +
    
    theme(
      plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20), color = "grey50"),
      strip.text = element_text(face = "bold", size = 14, margin = margin(b = 15)),
      legend.position = "none",
      panel.spacing = unit(2, "lines")
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
  
  # Flows
  geom_alluvium(aes(fill = `Difficulty Level`), width = 1/8, alpha = 0.7, curve_type = "cubic") +
  
  # Stratum (The boxes)
  geom_stratum(width = 1/8, fill = "grey95", color = "grey40", size = 0.3) +
  
  # Labels with Percentages
  # We use 'prop' which is automatically calculated by the stat layer
  geom_text(stat = "stratum", 
            aes(label = paste0(after_stat(stratum), "\n", 
                               scales::percent(after_stat(prop), accuracy = 1))), 
            size = 3, fontface = "bold", lineheight = 0.8) +
  
  # Scales & Theme
  scale_x_discrete(limits = c("Experience", "Workout", "Burn Intensity"), expand = c(.05, .05)) +
  scale_fill_brewer(palette = "Dark2", name = "Experience Group") + 
  theme_void() +
  labs(title = "The Warrior's Path", subtitle = "Tracking the flow: Experience Level → Workout Choice → Caloric Efficiency") +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16), plot.margin = margin(10, 10, 10, 10))

print(p_alluvial)


## --- PLOT 4: ENGINE HEATMAP (Static Replacement) ---
# Goal: Visualize the relationship between Intensity (BPM) and Output (Calories) 
# using density contours to handle large data volume.

p_static <- ggplot(df, aes(x = Avg_BPM, y = Calories_Burned)) +
  
  # create the density "heat" zones
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "white", size = 0.1) +
  
  # Facet by workout type to separate the "engines"
  facet_wrap(~ Workout_Type, ncol = 2) +
  
  # Colors: From 'cool' blue to 'hot' red
  scale_fill_viridis_c(option = "inferno", name = "Density") +
  
  theme_minimal(base_family = "Geologica") +
  
  labs(title = "Engine Combustion Zones", 
       subtitle = "Intensity (Heart Rate) vs. Output (Calories) Distribution",
       x = "Average Heart Rate (BPM)", 
       y = "Calories Burned") +
  
  theme(
    plot.title = element_text(face = "bold", size = 18),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(p_static)



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

dumbbell_data <- df %>%
  group_by(`Difficulty Level`) %>%
  summarise(
    Resting = mean(Resting_BPM, na.rm = TRUE),
    Max = mean(Max_BPM, na.rm = TRUE),
    Avg_Workout = mean(Avg_BPM, na.rm = TRUE)
  )

p_dumbbell <- ggplot(dumbbell_data, aes(y = `Difficulty Level`)) +
  
  # Reserve Bar (Grey)
  geom_segment(aes(x = Resting, xend = Max, yend = `Difficulty Level`), 
               color = "grey90", linewidth = 6, lineend = "round") +
  
  # Usage Bar (Green)
  geom_segment(aes(x = Resting, xend = Avg_Workout, yend = `Difficulty Level`), 
               color = "#69b3a2", linewidth = 6, lineend = "round") +
  
  # Points
  geom_point(aes(x = Resting), color = "#2c3e50", size = 5) +
  geom_point(aes(x = Max), color = "#e74c3c", size = 5) +
  geom_point(aes(x = Avg_Workout), color = "white", size = 3) +
  
  # Labels
  
  # Resting Label
  geom_text(aes(x = Resting, label = paste0(round(Resting), " bpm")), 
            vjust = 2.5, size = 3, fontface = "bold", color = "#2c3e50") +
  
  # Max Label
  geom_text(aes(x = Max, label = paste0(round(Max), " bpm")), 
            vjust = 2.5, size = 3, fontface = "bold", color = "#e74c3c") +
  
  # Train Avg Label (UPDATED: Bold and Size 3)
  geom_text(aes(x = Avg_Workout, label = paste0("Avg: ", round(Avg_Workout), " bpm")), 
            vjust = -1.8, size = 3, fontface = "bold", color = "#69b3a2") +
  
  # Reverse Y-axis order (Beginner on Top)
  scale_y_discrete(limits = rev) +
  
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16), 
        panel.grid.major.y = element_blank(), 
        axis.text.y = element_text(face = "bold", size = 11)) +
  
  labs(title = "Heart Rate Reserve Analysis", 
       subtitle = " Green Bar: Actual Training Intensity Used | Grey Bar: Total Reserve", 
       x = "Heart Rate (BPM)", y = "")

print(p_dumbbell)


# --- PLOT 7: RIDGELINE (Mountains of Fat) ---

ridge_data <- df %>%
  mutate(
    Group_Label = paste(Gender, "|", `Difficulty Level`),
    
    # REORDERING: Advanced at Bottom (Level 1), Beginner at Top
    Group_Label = factor(Group_Label, levels = c(
      "Male | Advanced", "Female | Advanced",
      "Male | Intermediate", "Female | Intermediate",
      "Male | Beginner", "Female | Beginner"
    ))
  ) %>%
  filter(!is.na(Group_Label))

p_ridge <- ggplot(ridge_data, aes(x = Fat_Percentage, y = Group_Label, fill = after_stat(x))) +
  
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.01, 
                               quantile_lines = TRUE, quantiles = 2, 
                               size = 0.3, color = "white") +
  
  scale_fill_viridis_c(name = "Body Fat %", option = "C") +
  
  theme_minimal() +
  
  theme(plot.title = element_text(face = "bold", size = 16), 
        axis.text.y = element_text(face = "bold", size = 10), 
        panel.grid.major.y = element_blank()) +
  
  labs(title = "Mountains of Fat", 
       subtitle = "Height of mountain = Density (Relative number of people)\nWhite line = Median value", 
       x = "Body Fat Percentage", 
       y = "") # REMOVED Y-AXIS TITLE

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


# --- PLOT 8: CORRELATION MATRIX (Final - Cleaned) ---

corr_data <- df %>%
  select(
    Age, 
    `Weight (kg)`, 
    BMI, 
    Calories_Burned, 
    Avg_BPM, 
    Duration_Hrs,
    # Kept the good ones, removed Body Fat
    Resting_BPM,
    `Workout_Frequency (days/week)`
  ) %>%
  rename(
    Weight = `Weight (kg)`, 
    Duration = Duration_Hrs,
    `Resting HR` = Resting_BPM,
    Frequency = `Workout_Frequency (days/week)`
  ) %>%
  cor(use = "complete.obs")

p_corr <- ggcorrplot(
  corr_data,
  method = "circle",       
  type = "lower",          
  hc.order = TRUE,         
  lab = TRUE,              
  lab_size = 3.5,            
  outline.color = "white", 
  colors = c("#69b3a2", "white", "#e74c3c"), 
  title = "Correlation Matrix",
  ggtheme = theme_minimal(base_family = "Geologica") 
) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid = element_blank()
  )

print(p_corr)