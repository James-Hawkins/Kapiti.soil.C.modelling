import pandas as pd
import re
import openpyxl

# Paths
master_xlsm_path = 'Weighted_species_params_master.xlsm'
child_xlsx_path = 'Weighted_species_params_child.xlsx'
xml_path = '../KE_Kapiti_speciesparameters_GLOBAL.xml'

# 1. Load the master file
print(f"Reading from master: {master_xlsm_path}")
df_master = pd.read_excel(master_xlsm_path)
params = df_master.set_index('code')

# Define species and groups
grass_species = ['cynodon_dactylon', 'cenchrus_mezianum', 'themeda_triandra']
forb_species = ['indigofera_volkensii', 'tephrosia_pumilla']
shrub_species = ['vechellia_drepanolobium']
all_species = grass_species + forb_species + shrub_species

# Get fractional covers for weighting
covers = params.loc['fractionalcover', all_species].to_dict()
for s in all_species:
    covers[s] = float(covers[s]) if pd.notna(covers[s]) else 0.0

grass_total_w = sum(covers[s] for s in grass_species)
forb_total_w = sum(covers[s] for s in forb_species)
shrub_total_w = sum(covers[s] for s in shrub_species)
site_total_w = grass_total_w + forb_total_w + shrub_total_w

# 2. Recalculate Hybrid columns
print("Recalculating hybrid averages...")
for code in params.index:
    if code in ['fractionalcover', 'weight.all', 'weight.ft', 'tree.density', 'dbh', 'treenumber']:
        continue
    
    if grass_total_w > 0:
        val = sum(params.loc[code, s] * covers[s] for s in grass_species if pd.notna(params.loc[code, s])) / grass_total_w
        params.at[code, 'HYBRID.GRASS'] = val
    if forb_total_w > 0:
        val = sum(params.loc[code, s] * covers[s] for s in forb_species if pd.notna(params.loc[code, s])) / forb_total_w
        params.at[code, 'HYBRID.FORB'] = val
    if shrub_total_w > 0:
        val = sum(params.loc[code, s] * covers[s] for s in shrub_species if pd.notna(params.loc[code, s])) / shrub_total_w
        params.at[code, 'HYBRID.SHRUB'] = val
    if site_total_w > 0:
        val = sum(params.loc[code, s] * covers[s] for s in all_species if pd.notna(params.loc[code, s])) / site_total_w
        params.at[code, 'HYBRID.ALL'] = val

# 3. Enforce FRACTION_FRUIT logic (< 1 total fraction constraint)
print("\nEnforcing Biomass Fraction Rule (Foliage + Root + Fruit < 1)...")
target_mappings = {
    'GRASS.HYBRID.1': 'HYBRID.GRASS',
    'FORB.HYBRID.1': 'HYBRID.FORB',
    'HYBRID_SHRUB': 'HYBRID.SHRUB',
    'HYBRID.SHRUB': 'HYBRID.SHRUB',
    'RED_OAT': 'themeda_triandra',
    'INDIGO': 'indigofera_volkensii',
    'WHISTL_THORN_manual': 'vechellia_drepanolobium',
    'WHISTL_THORN2': 'vechellia_drepanolobium',
    'HYBRID.ALL': 'HYBRID.ALL'
}

for col in set(target_mappings.values()):
    if col in params.columns:
        fol = params.loc['FRACTION_FOLIAGE', col] if pd.notna(params.loc['FRACTION_FOLIAGE', col]) else 0.0
        root = params.loc['FRACTION_ROOT', col] if pd.notna(params.loc['FRACTION_ROOT', col]) else 0.0
        fruit = params.loc['FRACTION_FRUIT', col] if pd.notna(params.loc['FRACTION_FRUIT', col]) else 0.0
        
        total = fol + root + fruit
        if total >= 1.0:
            new_fruit = max(0.0, 0.999 - fol - root)
            params.loc['FRACTION_FRUIT', col] = new_fruit
            print(f"  -> [{col}] Adjusted FRACTION_FRUIT from {fruit:.4f} to {new_fruit:.4f} (Original Sum: {total:.4f})")

# 4. Save to CHILD file
params.reset_index().to_excel(child_xlsx_path, index=False)
print(f"\nSUCCESS: {child_xlsx_path} updated.")

# 5. Update XML
mapping = {
    'AEJM': 'AEJM', 'AEKC': 'AEKC', 'AEKO': 'AEKO', 'AERD': 'AERD', 'AEVC': 'AEVC', 'AEVO': 'AEV0',
    'KC25': 'KC25', 'KM20': 'KM20', 'VCMAX25': 'VCMAX25', 'THETA': 'THETA',
    'GSMAX': 'GSMAX', 'GSMIN': 'GSMIN', 'WUECMAX': 'WUECMAX', 'WUECMIN': 'WUECMIN',
    'H2OREF_A': 'H2OREF_A', 'H2OREF_SENESCENCE': 'H2OREF_SENESCENCE', 'H2OREF_GS': 'H2OREF_GS',
    'H2OREF_FLUSHING': 'H2OREF_FLUSHING', 'H2OREF_LEAF_GROWTH': 'H2OREF_LEAF_GROWTH',
    'GDD_BASE_TEMPERATURE': 'GDD_BASE_TEMPERATURE', 'GDD_EMERG': 'GDD_EMERGENCE',
    'GDD_MATURITY': 'GDD_MATURITY', 'GDD_STEM_ELONGATION': 'GDD_STEM_ELONGATION',
    'GDD_ROOTS_GROWN': 'GDD_ROOTS_GROWN', 'GDDFOLSTART': 'GDDFOLSTART', 'GDDFOLEND': 'GDDFOLEND',
    'NDFLUSH': 'NDFLUSH', 'NDMORTA': 'NDMORTA', 'FRACTION_ROOT': 'FRACTION_ROOT',
    'FRACTION_FOLIAGE': 'FRACTION_FOLIAGE', 'FRACTION_FRUIT': 'FRACTION_FRUIT',
    'MFOLOPT': 'MFOLOPT', 'MWFM': 'MWFM', 'SLAMAX': 'SLAMAX', 'SLAMIN': 'SLAMIN', 'KO25': 'KO25'
}

with open(xml_path, 'r') as f:
    xml_content = f.read()

print("Synchronizing XML blocks (Respecting 'include.xml' filter)...")
for mnemonic, col in target_mappings.items():
    if col not in params.columns:
        continue
    
    pattern_block = fr'(<species[^>]*mnemonic="{mnemonic}"[^>]*>)(.*?)(</species>)'
    match = re.search(pattern_block, xml_content, re.DOTALL)
    if not match:
        pattern_block = fr'(<species[^>]*mnemonic\s*=\s*"{mnemonic}"[^>]*>)(.*?)(</species>)'
        match = re.search(pattern_block, xml_content, re.DOTALL)
        
    if not match:
        continue
    
    header, body, footer = match.groups()
    new_body = body
    updates = 0
    
    for ex_code, xml_name in mapping.items():
        if ex_code in params.index:
            # NEW RULE: Check include.xml
            include_val = params.loc[ex_code, 'include.xml']
            # Treat numeric 1.0 or boolean True as included
            if include_val == 1.0 or include_val is True:
                val = params.loc[ex_code, col]
                if pd.notna(val):
                    val_str = str(int(val)) if val == int(val) else f"{val:.6g}"
                    par_pattern = fr'(<par name="{xml_name}" value=")([^"]*)("(?:\s*/?>|\s*></par>))'
                    if re.search(par_pattern, new_body):
                        new_body = re.sub(par_pattern, fr'\g<1>{val_str}\g<3>', new_body)
                        updates += 1

    xml_content = xml_content.replace(match.group(0), header + new_body + footer)
    print(f"  -> XML Updated: {mnemonic} ({updates} parameters)")

with open(xml_path, 'w') as f:
    f.write(xml_content)

print("\nSUCCESS: XML file synchronized. 'include.xml' = FALSE parameters were skipped.")
