import pandas as pd
import numpy as np

backup_path = 'Species_params_GEMINI_populated.csv.backup'
xlsm_path = 'Species_params_GEMINI.xlsm'

df_backup = pd.read_csv(backup_path, encoding='utf-8-sig')
df_new = pd.read_excel(xlsm_path, engine='openpyxl')

code_col_new = 'code' if 'code' in df_new.columns else 'Code'
species_col_new = 'scientific_name' if 'scientific_name' in df_new.columns else df_new.columns[2]

updates = 0
for idx, row in df_new.iterrows():
    mid = str(row.get('midpoint')).strip()
    # allow '0' and '0.0'
    if mid and mid.lower() != 'nan' and mid != '' and mid != 'None':
        code = str(row.get(code_col_new)).strip()
        species = str(row.get(species_col_new)).strip()
        
        # We need a softer match for the species names to avoid false negatives.
        # Let's normalize both
        def norm(s): return str(s).lower().replace(" ", "").strip()
        
        mask = (df_backup['code'].astype(str).str.strip() == code)
        
        # Check species
        matched_idx = None
        for b_idx, b_row in df_backup[mask].iterrows():
            if norm(species) in norm(b_row['scientific_name']) or norm(b_row['scientific_name']) in norm(species):
                matched_idx = b_idx
                break
                
        if matched_idx is not None:
            df_backup.at[matched_idx, 'minimum'] = row.get('minimum', '')
            df_backup.at[matched_idx, 'maximum'] = row.get('maximum', '')
            df_backup.at[matched_idx, 'point_estimate'] = row.get('point_estimate', '')
            df_backup.at[matched_idx, 'midpoint'] = row.get('midpoint', '')
            df_backup.at[matched_idx, 'references'] = row.get('references', '')
            updates += 1

print(f"Extracted {updates} parameters from XLSM and merged into backup dataset.")

df_backup.to_csv('Species_params_GEMINI_populated.csv', index=False, encoding='utf-8-sig')
