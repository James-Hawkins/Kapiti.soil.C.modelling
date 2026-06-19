import csv

file_path = r'C:\Users\JHawkins\Documents\Github\Soil.C.modelling\Kapiti.soil.C\projects\KE_Kapiti_Eddy_July\data\Species_params_GEMINI.csv'

with open(file_path, 'r', encoding='utf-8') as f:
    reader = csv.reader(f)
    rows = list(reader)

for i in range(70, 85):
    if i < len(rows):
        r = rows[i]
        code = r[9] if len(r) > 9 else "N/A"
        name = r[1] if len(r) > 1 else "N/A"
        print(f"Row {i+1}: {code} ({name})")
