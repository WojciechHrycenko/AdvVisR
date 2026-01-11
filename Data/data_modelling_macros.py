import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# 1. Load Data
try:
    df = pd.read_csv('Final_data.csv')
except FileNotFoundError:
    df = pd.DataFrame()

# --- STEP 0: FORCE BALANCED POPULATION & SYNC EXPERIENCE ---

# 1. Balansujemy udziały grup (Reset Difficulty Level)
# Dzięki temu mamy pewność, że grupy nie znikną.
# Ustawiam: 30% Beginner, 40% Intermediate, 30% Advanced
population_probs = [0.35, 0.45, 0.20]
levels = ['Beginner', 'Intermediate', 'Advanced']
df['Difficulty Level'] = np.random.choice(levels, size=len(df), p=population_probs)

# 2. Synchronizujemy Experience_Level z nowym Difficulty Level
def sync_experience(row):
    d = row['Difficulty Level']
    noise = np.random.uniform(-0.3, 0.3)
    
    if d == 'Beginner': 
        return 1.0 + abs(noise) # 1.0 - 1.3
    elif d == 'Intermediate': 
        return 2.0 + noise      # 1.7 - 2.3
    elif d == 'Advanced': 
        return 3.0 + noise      # 2.7 - 3.3
    return 2.0

df['Experience_Level'] = df.apply(sync_experience, axis=1)


# --- STEP A: CATEGORICAL REDISTRIBUTION ---

# A.1 Meal Frequency
meal_freq_probs = {
    'Beginner':     {'Breakfast': 0.10, 'Lunch': 0.35, 'Dinner': 0.40, 'Snack': 0.15}, 
    'Intermediate': {'Breakfast': 0.25, 'Lunch': 0.35, 'Dinner': 0.25, 'Snack': 0.15}, 
    'Advanced':     {'Breakfast': 0.30, 'Lunch': 0.30, 'Dinner': 0.20, 'Snack': 0.20}  
}
def reassign_meal_type(row):
    diff = row['Difficulty Level']
    if diff not in meal_freq_probs: return row['meal_type']
    probs = meal_freq_probs[diff]
    return np.random.choice(list(probs.keys()), p=list(probs.values()))
df['meal_type'] = df.apply(reassign_meal_type, axis=1)

# A.2 Intensity
intensity_probs = {
    'Beginner':     {'Low': 0.45, 'Medium': 0.35, 'High': 0.15, 'Very High': 0.05},
    'Intermediate': {'Low': 0.20, 'Medium': 0.40, 'High': 0.30, 'Very High': 0.10},
    'Advanced':     {'Low': 0.05, 'Medium': 0.15, 'High': 0.40, 'Very High': 0.40}
}
def reassign_intensity(row):
    diff = row['Difficulty Level']
    if diff not in intensity_probs: return row['Burns_Calories_Bin']
    probs = intensity_probs[diff]
    return np.random.choice(list(probs.keys()), p=list(probs.values()))
df['Burns_Calories_Bin'] = df.apply(reassign_intensity, axis=1)

# A.3 Workout Type (With Gender/Age Logic included)
def reassign_workout(row):
    gender = row['Gender']
    age = row['Age']
    
    # Domyślne wagi
    probs = {'Cardio': 0.25, 'Yoga': 0.25, 'Strength': 0.25, 'HIIT': 0.25}
    
    # 1. Młodzi (18-35)
    if age <= 35:
        if gender == 'Male':
            probs = {'Strength': 0.45, 'HIIT': 0.25, 'Cardio': 0.20, 'Yoga': 0.10}
        else: # Female
            probs = {'Cardio': 0.30, 'Yoga': 0.30, 'HIIT': 0.25, 'Strength': 0.15}
            
    # 2. Średni wiek (36-55)
    elif 35 < age <= 55:
        if gender == 'Male':
            probs = {'Strength': 0.35, 'Cardio': 0.35, 'HIIT': 0.15, 'Yoga': 0.15}
        else: # Female
            probs = {'Yoga': 0.40, 'Cardio': 0.35, 'Strength': 0.15, 'HIIT': 0.10}
            
    # 3. Starsi (55+)
    else:
        probs = {'Cardio': 0.40, 'Yoga': 0.40, 'Strength': 0.15, 'HIIT': 0.05}
    
    return np.random.choice(list(probs.keys()), p=list(probs.values()))

df['Workout_Type'] = df.apply(reassign_workout, axis=1)

# A.4 Cooking Method
cooking_probs = {
    'Beginner': {
        'breakfast': {'Fried': 0.4, 'Boiled': 0.2, 'Raw': 0.2, 'Baked': 0.2, 'Steamed':0, 'Roasted':0, 'Grilled':0},
        'lunch':     {'Fried': 0.4, 'Baked': 0.2, 'Roasted': 0.1, 'Grilled': 0.1, 'Boiled': 0.1, 'Steamed': 0.1, 'Raw': 0},
        'dinner':    {'Fried': 0.3, 'Baked': 0.3, 'Roasted': 0.2, 'Boiled': 0.1, 'Grilled': 0.1, 'Steamed': 0, 'Raw': 0},
        'snack':     {'Raw': 0.4, 'Fried': 0.3, 'Baked': 0.3, 'Boiled':0, 'Steamed':0, 'Roasted':0, 'Grilled':0}
    },
    'Intermediate': {
        'breakfast': {'Boiled': 0.4, 'Raw': 0.3, 'Fried': 0.1, 'Baked': 0.1, 'Steamed': 0.1, 'Roasted': 0, 'Grilled': 0},
        'lunch':     {'Grilled': 0.4, 'Baked': 0.3, 'Roasted': 0.1, 'Steamed': 0.1, 'Boiled': 0.05, 'Fried': 0.05, 'Raw': 0},
        'dinner':    {'Baked': 0.3, 'Roasted': 0.3, 'Grilled': 0.2, 'Steamed': 0.1, 'Boiled': 0.1, 'Fried': 0, 'Raw': 0},
        'snack':     {'Raw': 0.7, 'Baked': 0.2, 'Boiled': 0.1, 'Fried': 0, 'Steamed': 0, 'Roasted': 0, 'Grilled': 0}
    },
    'Advanced': {
        'breakfast': {'Boiled': 0.5, 'Raw': 0.3, 'Steamed': 0.2, 'Fried': 0, 'Baked': 0, 'Roasted': 0, 'Grilled': 0},
        'lunch':     {'Steamed': 0.4, 'Grilled': 0.3, 'Boiled': 0.2, 'Roasted': 0.1, 'Baked': 0, 'Fried': 0, 'Raw': 0},
        'dinner':    {'Steamed': 0.5, 'Grilled': 0.2, 'Boiled': 0.2, 'Raw': 0.1, 'Baked': 0, 'Roasted': 0, 'Fried': 0},
        'snack':     {'Raw': 0.9, 'Boiled': 0.1, 'Steamed': 0, 'Baked': 0, 'Fried': 0, 'Roasted': 0, 'Grilled': 0}
    }
}
def assign_cooking_method(row):
    diff = row['Difficulty Level']
    meal = str(row['meal_type']).lower()
    if diff not in cooking_probs: diff = 'Intermediate'
    if meal not in cooking_probs[diff]: return row['cooking_method']
    probs = cooking_probs[diff][meal]
    return np.random.choice(list(probs.keys()), p=list(probs.values()))
df['cooking_method'] = df.apply(assign_cooking_method, axis=1)

# A.5 Workout Frequency (Natural Gaussian)
freq_params = {
    'Beginner':     {'mean': 2.5, 'std': 1.0},
    'Intermediate': {'mean': 4.0, 'std': 1.0},
    'Advanced':     {'mean': 5.5, 'std': 1.0}
}
def reassign_frequency(row):
    diff = row['Difficulty Level']
    params = freq_params.get(diff, {'mean': 3.5, 'std': 1.0})
    freq = np.random.normal(params['mean'], params['std'])
    return np.clip(freq, 1.0, 7.0)
df['Workout_Frequency (days/week)'] = df.apply(reassign_frequency, axis=1)


# --- STEP B: NUMERICAL REDISTRIBUTION ---

# B.1 Performance Metrics
perf_params = {
    'HIIT':     {'bpm': (145, 180), 'dur': (0.5, 1.0), 'eff': 6.0},
    'Cardio':   {'bpm': (130, 160), 'dur': (1.0, 2.5), 'eff': 5.0},
    'Strength': {'bpm': (110, 145), 'dur': (0.8, 1.8), 'eff': 4.5},
    'Yoga':     {'bpm': (80, 110),  'dur': (0.8, 1.5), 'eff': 3.0}
}
diff_mod = {
    'Beginner':     {'bpm': 0.90, 'dur': 0.8},
    'Intermediate': {'bpm': 1.00, 'dur': 1.0},
    'Advanced':     {'bpm': 1.05, 'dur': 1.2}
}
def simulate_performance(row):
    w_type = row['Workout_Type']
    diff = row['Difficulty Level']
    if w_type not in perf_params: return row[['Avg_BPM', 'Session_Duration (hours)', 'Calories_Burned']]
    params = perf_params[w_type]
    mods = diff_mod.get(diff, {'bpm': 1.0, 'dur': 1.0})
    base_dur = np.random.uniform(params['dur'][0], params['dur'][1])
    duration = base_dur * mods['dur']
    base_bpm = np.random.uniform(params['bpm'][0], params['bpm'][1])
    bpm = base_bpm * mods['bpm']
    noise = np.random.uniform(0.9, 1.1)
    calories = duration * bpm * params['eff'] * noise
    return pd.Series([np.clip(bpm, 50, 200), np.clip(duration, 0.2, 4.0), np.clip(calories, 50, 2500)])
df[['Avg_BPM', 'Session_Duration (hours)', 'Calories_Burned']] = df.apply(simulate_performance, axis=1)

# B.2 Macro Nutrients
BASELINES = {'Proteins': 100.0, 'Fats': 66.5, 'Carbs': 250.0}
gender_mult = {'Male': {'Proteins': 1.15, 'Fats': 1.10, 'Carbs': 1.15}, 'Female': {'Proteins': 0.85, 'Fats': 0.90, 'Carbs': 0.85}}
diff_mult = {'Beginner': {'Proteins': 0.90, 'Fats': 1.05, 'Carbs': 0.80}, 'Intermediate': {'Proteins': 1.00, 'Fats': 1.00, 'Carbs': 1.00}, 'Advanced': {'Proteins': 1.25, 'Fats': 0.90, 'Carbs': 1.30}}
workout_mult = {'Strength': {'Proteins': 1.35, 'Fats': 1.10, 'Carbs': 0.90}, 'HIIT': {'Proteins': 1.15, 'Fats': 0.95, 'Carbs': 1.20}, 'Cardio': {'Proteins': 1.00, 'Fats': 0.90, 'Carbs': 1.40}, 'Yoga': {'Proteins': 0.95, 'Fats': 1.00, 'Carbs': 0.95}}
meal_mult = {'breakfast': {'Proteins': 0.70, 'Fats': 0.90, 'Carbs': 1.10}, 'lunch': {'Proteins': 1.30, 'Fats': 1.20, 'Carbs': 1.25}, 'dinner': {'Proteins': 1.35, 'Fats': 1.10, 'Carbs': 0.85}, 'snack': {'Proteins': 0.25, 'Fats': 0.85, 'Carbs': 0.85}}
cooking_nutrient_mult = {'Fried': {'Proteins': 1.0, 'Fats': 1.40, 'Carbs': 1.10}, 'Roasted': {'Proteins': 1.0, 'Fats': 1.15, 'Carbs': 1.00}, 'Baked': {'Proteins': 1.0, 'Fats': 1.10, 'Carbs': 1.00}, 'Grilled': {'Proteins': 1.0, 'Fats': 1.05, 'Carbs': 1.00}, 'Boiled': {'Proteins': 1.0, 'Fats': 0.90, 'Carbs': 1.00}, 'Steamed': {'Proteins': 1.0, 'Fats': 0.90, 'Carbs': 1.00}, 'Raw': {'Proteins': 1.0, 'Fats': 0.95, 'Carbs': 1.00}}

def advanced_modeling_full(df):
    for col, base in BASELINES.items():
        if df[col].mean() != 0:
            df[col] = (df[col] / df[col].mean()) * base
    def apply_factors(row):
        p, f, c = row['Proteins'], row['Fats'], row['Carbs']
        g = gender_mult.get(row['Gender'], {'Proteins':1, 'Fats':1, 'Carbs':1}); p*=g['Proteins']; f*=g['Fats']; c*=g['Carbs']
        d = diff_mult.get(row['Difficulty Level'], {'Proteins':1, 'Fats':1, 'Carbs':1}); p*=d['Proteins']; f*=d['Fats']; c*=d['Carbs']
        w = workout_mult.get(row['Workout_Type'], {'Proteins':1, 'Fats':1, 'Carbs':1}); p*=w['Proteins']; f*=w['Fats']; c*=w['Carbs']
        m = meal_mult.get(str(row['meal_type']).lower(), {'Proteins':1, 'Fats':1, 'Carbs':1}); p*=m['Proteins']; f*=m['Fats']; c*=m['Carbs']
        cm = cooking_nutrient_mult.get(row['cooking_method'], {'Proteins':1, 'Fats':1, 'Carbs':1}); p*=cm['Proteins']; f*=cm['Fats']; c*=cm['Carbs']
        return pd.Series([p, f, c])
    df[['Proteins', 'Fats', 'Carbs']] = df.apply(apply_factors, axis=1)
    for col in ['Proteins', 'Fats', 'Carbs']:
        noise = np.random.uniform(0.85, 1.15, size=len(df))
        df[col] = df[col] * noise
    return df

df_final = advanced_modeling_full(df)

# --- STEP C: ADDITIONAL PRO MODELING (FULL LOGIC) ---

# C.1 Diet-Based Macro Restructuring & Calorie Calculation
# - Adjusts Macros based on Diet Type (Keto = High Fat, etc.)
# - Applies Aggressive "Meal Volume Bias" to visually distinguish diets in Treemaps
# - Calculates Sugar with Toned Down logic (to avoid "all red" charts)
# - Re-calculates Calories strictly from Macros (4/9/4 rule)

def adjust_nutrition_by_diet(row):
    p, f, c = row['Proteins'], row['Fats'], row['Carbs']
    diet = str(row['diet_type'])
    meal = str(row['meal_type']).lower()
    
    # 1. MACRO PROFILE (Base Proportions)
    if 'Keto' in diet:
        p *= 1.0;  f *= 1.7;  c *= 0.05   # High Fat, Very Low Carb
    elif 'Vegan' in diet or 'Vegetarian' in diet:
        p *= 0.8;  f *= 0.7;  c *= 1.40   # High Carb, Lower Fat/Protein
    elif 'Paleo' in diet:
        p *= 1.5;  f *= 1.1;  c *= 0.30   # High Protein
    elif 'Low-Carb' in diet:
        p *= 1.2;  f *= 1.2;  c *= 0.40
    # Balanced stays x1.0
        
    # 2. MEAL VOLUME BIAS (Aggressive Stereotypes for Visualization)
    volume_factor = 1.0
    
    if 'Keto' in diet:
        # IF Style: Huge Lunch, tiny Breakfast/Snack
        if meal == 'lunch': volume_factor = 1.6
        elif meal == 'dinner': volume_factor = 1.0
        elif meal == 'breakfast': volume_factor = 0.5
        elif meal == 'snack': volume_factor = 0.2
        
    elif 'Paleo' in diet:
        # Feast style: Huge Dinner
        if meal == 'dinner': volume_factor = 1.8
        elif meal == 'lunch': volume_factor = 0.8
        elif meal == 'breakfast': volume_factor = 0.6
        elif meal == 'snack': volume_factor = 0.4
        
    elif 'Vegan' in diet or 'Vegetarian' in diet:
        # Grazing style: Big Snacks & Breakfasts
        if meal == 'breakfast': volume_factor = 1.4
        elif meal == 'snack': volume_factor = 1.5
        elif meal == 'lunch': volume_factor = 0.9
        elif meal == 'dinner': volume_factor = 0.9
        
    elif 'Low-Carb' in diet:
        # Tapering down
        if meal == 'lunch': volume_factor = 1.3
        elif meal == 'breakfast': volume_factor = 1.1
        elif meal == 'dinner': volume_factor = 0.7
        elif meal == 'snack': volume_factor = 0.5
        
    else: # Balanced
        # Classic pyramid
        if meal == 'lunch': volume_factor = 1.2
        elif meal == 'dinner': volume_factor = 1.1
        elif meal == 'breakfast': volume_factor = 0.9
        elif meal == 'snack': volume_factor = 0.6

    p *= volume_factor
    f *= volume_factor
    c *= volume_factor

    # 3. SUGAR LOGIC (TONED DOWN)
    # Reduced base calculation from 15% to 10% of carbs
    base_sugar = c * 0.10
    
    # Milder multipliers
    if 'Keto' in diet: base_sugar *= 0.05
    elif 'Vegan' in diet or 'Vegetarian' in diet: base_sugar *= 1.2
    elif 'Paleo' in diet: base_sugar *= 0.4
    
    # Milder snack penalty
    if meal == 'snack': base_sugar *= 1.3
    
    # Random noise but cap max sugar realistically
    sugar = min(c, max(0, base_sugar * np.random.uniform(0.8, 1.2)))
    
    # 4. Strict Calorie Calc (4/9/4 Rule)
    calories = (p * 4) + (f * 9) + (c * 4)
    
    return pd.Series([p, f, c, sugar, calories])

df_final[['Proteins', 'Fats', 'Carbs', 'sugar_g', 'Calories']] = df_final.apply(adjust_nutrition_by_diet, axis=1)


# C.2 Heart Rate Physiology (Resting & Max based on Experience)
# - Advanced users: Lower resting BPM, higher Max BPM (Athletic heart)
def model_physiology(row):
    diff = row['Difficulty Level']
    age = row['Age']
    
    # Tanaka formula for base max
    base_max_hr = 208 - (0.7 * age)
    
    if diff == 'Advanced':
        resting = np.random.normal(52, 5)  
        max_hr = base_max_hr * np.random.uniform(0.98, 1.05) 
    elif diff == 'Intermediate':
        resting = np.random.normal(68, 6)
        max_hr = base_max_hr * np.random.uniform(0.92, 1.0)
    else: # Beginner
        resting = np.random.normal(78, 8)
        max_hr = base_max_hr * np.random.uniform(0.85, 0.95)
        
    return pd.Series([resting, max_hr])

df_final[['Resting_BPM', 'Max_BPM']] = df_final.apply(model_physiology, axis=1)


# C.3 Fat Percentage (Body Composition)
# - Gender differences + Experience bonus (Advanced = leaner)
def model_body_fat(row):
    gender = row['Gender']
    diff = row['Difficulty Level']
    
    if gender == 'Male':
        fat = np.random.normal(20, 4)
        if diff == 'Advanced': fat -= 6
        elif diff == 'Intermediate': fat -= 2
        min_fat = 4.0
    else: # Female
        fat = np.random.normal(28, 5)
        if diff == 'Advanced': fat -= 6
        elif diff == 'Intermediate': fat -= 2
        min_fat = 12.0
            
    return max(min_fat, fat)

df_final['Fat_Percentage'] = df_final.apply(model_body_fat, axis=1)


# C.4 Burn Category Logic
# - Syncs with the actual Calories_Burned column for Alluvial consistency
def set_burn_category(cals):
    if cals < 400: return 'Low Burn'
    elif cals < 750: return 'Med Burn'
    else: return 'High Burn'

df_final['Burn_Category'] = df_final['Calories_Burned'].apply(set_burn_category)

# --- STEP D: CORRELATION ENGINEERING (PHYSICS ENFORCEMENT PRO) ---
# Goal: Enforce logical correlations for the final matrix.
# We create dependencies: Age -> Weight -> Duration/BPM -> Calories.

def enforce_physics(row):
    # Load current values
    age = row['Age']
    weight = row['Weight (kg)']
    height = row['Height (m)']
    duration = row['Session_Duration (hours)']
    bpm = row['Avg_BPM']
    
    # 1. AGE -> WEIGHT / BMI LINK (Metabolic slowdown)
    # Older people in dataset get a slight weight penalty (Trend)
    # This creates Age-Weight and Age-BMI correlation.
    if age > 30:
        weight_trend = (age - 30) * 0.15 
        weight += np.random.uniform(0, weight_trend)
        
    # Recalculate BMI with new Weight
    if height < 1.0: height = 1.7
    bmi = weight / (height ** 2)
    
    # 2. WEIGHT/BMI -> DURATION (Fatigue Factor)
    # Heavier people might train slightly shorter (Negative Correlation)
    # If BMI is high, duration shrinks slightly.
    if bmi > 25:
        fatigue_factor = 1.0 - ((bmi - 25) * 0.015) # -1.5% duration per BMI point over 25
        duration *= max(0.5, fatigue_factor) # Don't go below 50%
        
    # 3. WEIGHT/BMI -> BPM (Effort Factor)
    # Heavier body needs higher Heart Rate to move (Positive Correlation)
    if bmi > 25:
        effort_add = (bmi - 25) * 0.8
        bpm += effort_add
        
    # 4. AGE -> BPM (Max HR Ceiling)
    # Older people can't sustain super high BPM (Negative Correlation)
    max_theoretical = 208 - (0.7 * age)
    if bpm > max_theoretical:
        bpm = max_theoretical * np.random.uniform(0.9, 0.98)
        
    # 5. FINAL CALORIE CALCULATION (The Master Formula)
    # Burn depends on all above: Mass, Duration, Intensity, Age
    
    # Intensity Factor (Exponential cost of high RPM)
    intensity_factor = (bpm / 100.0) ** 1.5
    
    # Mass Factor (Linear cost of moving weight)
    mass_factor = weight / 70.0
    
    # Age Factor (Metabolic decline) - Older burn slightly less
    age_factor = 1.0 - ((age - 20) * 0.002) 
    
    # Base constant (~450 kcal/hr baseline)
    base_burn = 450
    
    new_burn = base_burn * duration * intensity_factor * mass_factor * age_factor
    
    # Add small noise (3%) to preserve 'natural' look
    new_burn *= np.random.uniform(0.97, 1.03)
    
    return pd.Series([weight, bmi, duration, bpm, new_burn])

# Apply the physics engine and update ALL correlated columns
df_final[['Weight (kg)', 'BMI', 'Session_Duration (hours)', 'Avg_BPM', 'Calories_Burned']] = df_final.apply(enforce_physics, axis=1)

# --- RE-SYNC BURN CATEGORY ---
# Update category based on new physics-based calories
df_final['Burn_Category'] = df_final['Calories_Burned'].apply(set_burn_category)

# --- OPTIONAL: MUSCLE GROUP LOGIC (THE BLUEPRINT) ---
def assign_muscle_group(row):
    w_type = row['Workout_Type']
    
    if w_type == 'Cardio':
        return np.random.choice(['Legs', 'Full Body', 'Heart'], p=[0.4, 0.4, 0.2])
    elif w_type == 'HIIT':
        return np.random.choice(['Full Body', 'Legs', 'Core'], p=[0.6, 0.2, 0.2])
    elif w_type == 'Strength':
        return np.random.choice(['Chest', 'Back', 'Legs', 'Arms', 'Shoulders'], p=[0.2, 0.2, 0.3, 0.15, 0.15])
    elif w_type == 'Yoga':
        return np.random.choice(['Core', 'Full Body', 'Back'], p=[0.5, 0.3, 0.2])
    return 'Full Body'

df_final['Target Muscle Group'] = df_final.apply(assign_muscle_group, axis=1)
# --- SAVE ---
df_final.to_csv('Final_data_model.csv', index=False)