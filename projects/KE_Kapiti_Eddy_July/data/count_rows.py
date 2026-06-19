import csv
import os

file_path = r'C:\Users\JHawkins\Documents\Github\Soil.C.modelling\Kapiti.soil.C\projects\KE_Kapiti_Eddy_July\data\Species_params_GEMINI_populated.csv'

with open(file_path, 'r', encoding='utf-8') as f:
    lines = f.readlines()

row_count = 0
line_ptr = 0
while line_ptr < len(lines):
    row_count += 1
    row_str = ''
    start_line = line_ptr + 1
    while line_ptr < len(lines):
        row_str += lines[line_ptr]
        line_ptr += 1
        try:
            # Check if row_str is a complete CSV row
            parsed = list(csv.reader([row_str.strip()]))
            if parsed:
                row = parsed[0]
                break
        except (csv.Error, IndexError):
            continue
    end_line = line_ptr
    if start_line != row_count:
        code = row[9] if len(row) > 9 else "N/A"
        name = row[1] if len(row) > 1 else "N/A"
        print(f"Row {row_count}: Lines {start_line}-{end_line}, Code: {code}, Name: {name}")
    if row_count == 125: break
