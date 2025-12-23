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

# --- SAVE ---
df_final.to_csv('Final_data_modeled.csv', index=False)