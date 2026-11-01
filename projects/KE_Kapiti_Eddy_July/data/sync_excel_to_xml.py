import pandas as pd
import re
import openpyxl
import os

# Paths
master_xlsm_path = 'Weighted_species_params_master_temp.xlsm' if os.path.exists('Weighted_species_params_master_temp.xlsm') else 'Weighted_species_params_master.xlsm'
child_xlsx_path = 'Weighted_species_params_child.xlsx'
xml_path = '../KE_Kapiti_speciesparameters_GLOBAL.xml'
events_xml_path = '../KE_Kapiti_events_eddy.xml'

# 1. Load the master file
print(f"Reading from master: {master_xlsm_path}")
df_master = pd.read_excel(master_xlsm_path)
params = df_master.set_index('code')

# Define species and groups
grass_species = ['cynodon_dactylon', 'cenchrus_mezianum', 'themeda_triandra']
forb_species = [s for s in ['indigofera_volkensii', 'tephrosia', 'tephrosia_pumilla'] if s in params.columns]
shrub_species = [s for s in ['vachellia_drepanolobium', 'vechellia_drepanolobium'] if s in params.columns]
all_species = grass_species + forb_species

# Get weights from master file (weight.ft for functional type hybrids, weight.all for HYBRID.ALL)
weights_ft = {}
weights_all = {}
for s in grass_species + forb_species + shrub_species:
    val_ft = params.loc['weight.ft', s] if 'weight.ft' in params.index else None
    val_all = params.loc['weight.all', s] if 'weight.all' in params.index else None
    val_cov = params.loc['fractionalcover', s] if 'fractionalcover' in params.index else 0.0
    try:
        weights_ft[s] = float(val_ft) if pd.notna(val_ft) else float(val_cov)
    except (ValueError, TypeError):
        weights_ft[s] = 0.0
    try:
        weights_all[s] = float(val_all) if pd.notna(val_all) else float(val_cov)
    except (ValueError, TypeError):
        weights_all[s] = 0.0

# Since Vachellia drepanolobium is the single shrub species (weight.ft = 1), its weight in HYBRID.SHRUB is 1.0
for s in shrub_species:
    if weights_ft.get(s, 0.0) == 0.0:
        weights_ft[s] = 1.0

grass_total_w = sum(weights_ft[s] for s in grass_species)
forb_total_w = sum(weights_ft[s] for s in forb_species)
shrub_total_w = sum(weights_ft[s] for s in shrub_species)
site_total_w = sum(weights_all[s] for s in all_species)

# 2. Recalculate Hybrid columns
print("Recalculating hybrid averages...")
for code in params.index:
    if code in ['fractionalcover', 'weight.all', 'weight.ft', 'tree.density', 'dbh', 'treenumber', 'initialbiomass']:
        continue
    
    if grass_total_w > 0:
        vals = [float(params.loc[code, s]) * weights_ft[s] for s in grass_species if pd.notna(params.loc[code, s])]
        if vals:
            params.at[code, 'HYBRID.GRASS'] = sum(vals) / grass_total_w
    if forb_total_w > 0:
        vals = [float(params.loc[code, s]) * weights_ft[s] for s in forb_species if pd.notna(params.loc[code, s])]
        if vals:
            params.at[code, 'HYBRID.FORB'] = sum(vals) / forb_total_w
    if shrub_total_w > 0:
        vals = [float(params.loc[code, s]) * weights_ft[s] for s in shrub_species if pd.notna(params.loc[code, s])]
        if vals:
            params.at[code, 'HYBRID.SHRUB'] = sum(vals) / shrub_total_w
    if site_total_w > 0:
        vals = [float(params.loc[code, s]) * weights_all[s] for s in all_species if pd.notna(params.loc[code, s])]
        if vals:
            params.at[code, 'HYBRID.ALL'] = sum(vals) / site_total_w

# 3. Enforce FRACTION_FRUIT logic (< 1 total fraction constraint)
print("\nEnforcing Biomass Fraction Rule (Foliage + Root + Fruit < 1)...")
target_mappings = {
    'GRASS.HYBRID.1': 'HYBRID.GRASS',
    'FORB.HYBRID.1': 'HYBRID.FORB',
    'HYBRID_SHRUB': 'HYBRID.SHRUB',
    'HYBRID.SHRUB': 'HYBRID.SHRUB',
    'RED_OAT': 'themeda_triandra',
    'INDIGO': 'indigofera_volkensii',
    'WHISTL_THORN_manual': 'vachellia_drepanolobium',
    'WHISTL_THORN2': 'vachellia_drepanolobium',
    'HYBRID.ALL': 'HYBRID.ALL'
}

for col in set(target_mappings.values()):
    if col in params.columns:
        fol = float(params.loc['FRACTION_FOLIAGE', col]) if pd.notna(params.loc['FRACTION_FOLIAGE', col]) else 0.0
        root = float(params.loc['FRACTION_ROOT', col]) if pd.notna(params.loc['FRACTION_ROOT', col]) else 0.0
        fruit = float(params.loc['FRACTION_FRUIT', col]) if pd.notna(params.loc['FRACTION_FRUIT', col]) else 0.0
        
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
    'AEJM': ['AEJM'], 'AEKC': ['AEKC'], 'AEKO': ['AEKO'], 'AERD': ['AERD'], 'AEVC': ['AEVC'],
    'AEVO': ['AEV0', 'AEVO'],
    'KC25': ['KC25'], 'KM20': ['KM20'], 'VCMAX25': ['VCMAX25', 'VCMAX'], 'THETA': ['THETA'],
    'GSMAX': ['GSMAX'], 'GSMIN': ['GSMIN'], 'WUECMAX': ['WUECMAX'], 'WUECMIN': ['WUECMIN'],
    'H2OREF_A': ['H2OREF_A'], 'H2OREF_SENESCENCE': ['H2OREF_SENESCENCE'], 'H2OREF_GS': ['H2OREF_GS'],
    'H2OREF_FLUSHING': ['H2OREF_FLUSHING'], 'H2OREF_LEAF_GROWTH': ['H2OREF_LEAF_GROWTH'],
    'GDD_BASE_TEMPERATURE': ['GDD_BASE_TEMPERATURE'], 'GDD_EMERG': ['GDD_EMERGENCE', 'GDD_EMERG'],
    'GDD_MATURITY': ['GDD_MATURITY'], 'GDD_STEM_ELONGATION': ['GDD_STEM_ELONGATION'],
    'GDD_ROOTS_GROWN': ['GDD_ROOTS_GROWN'], 'GDDFOLSTART': ['GDDFOLSTART'], 'GDDFOLEND': ['GDDFOLEND'],
    'NDFLUSH': ['NDFLUSH'], 'NDMORTA': ['NDMORTA'], 'FRACTION_ROOT': ['FRACTION_ROOT'],
    'FRACTION_FOLIAGE': ['FRACTION_FOLIAGE'], 'FRACTION_FRUIT': ['FRACTION_FRUIT'],
    'FOLRELGROMAX': ['FOLRELGROMAX'], 'QJVC': ['QJVC'], 'QRD25': ['QRD25'],
    'MFOLOPT': ['MFOLOPT'], 'MWFM': ['MWFM'], 'SLAMAX': ['SLAMAX'], 'SLAMIN': ['SLAMIN'], 'KO25': ['KO25']
}

with open(xml_path, 'r', encoding='utf-8') as f:
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
    
    for ex_code, xml_names in mapping.items():
        if ex_code in params.index:
            include_val = params.loc[ex_code, 'include.xml']
            if include_val == 1.0 or include_val is True:
                val = params.loc[ex_code, col]
                if pd.notna(val):
                    fval = float(val)
                    val_str = str(int(fval)) if fval == int(fval) else f"{fval:.6g}"
                    updated_this_code = False
                    for xml_name in xml_names:
                        par_pattern = fr'(<par name="{xml_name}" value=")([^"]*)("(?:\s*/?>|\s*></par>))'
                        if re.search(par_pattern, new_body):
                            new_body = re.sub(par_pattern, fr'\g<1>{val_str}\g<3>', new_body)
                            updated_this_code = True
                    if not updated_this_code and mnemonic == 'HYBRID.SHRUB':
                        # Append missing parameter to HYBRID.SHRUB block
                        new_body = new_body.rstrip() + f'\n      <par name="{xml_names[0]}" value="{val_str}"/>\n    '
                        updated_this_code = True
                    if updated_this_code:
                        updates += 1

    xml_content = xml_content.replace(match.group(0), header + new_body + footer)
    print(f"  -> XML Updated: {mnemonic} ({updates} parameters)")

with open(xml_path, 'w', encoding='utf-8') as f:
    f.write(xml_content)

print("\nSUCCESS: Species XML file synchronized. 'include.xml' = FALSE parameters were skipped.")

# 6. Update Events XML (KE_Kapiti_events_eddy.xml)
events_target_mappings = {
    'HYBRID.ALL': 'HYBRID.ALL',
    'FORB.HYBRID.1': 'HYBRID.FORB',
    'HYBRID.FORB': 'HYBRID.FORB',
    'HYBRID.SHRUB': 'HYBRID.SHRUB',
    'HYBRID_SHRUB': 'HYBRID.SHRUB',
    'GRASS.HYBRID.1': 'HYBRID.GRASS',
    'HYBRID.GRASS': 'HYBRID.GRASS'
}
event_param_codes = ['initialbiomass', 'heightmax', 'fractionalcover', 'rootingdepth']

if os.path.exists(events_xml_path):
    with open(events_xml_path, 'r', encoding='utf-8') as f:
        events_content = f.read()

    print("\nSynchronizing Events XML blocks (KE_Kapiti_events_eddy.xml)...")
    for plant_type, col in events_target_mappings.items():
        if col not in params.columns:
            continue

        pattern_plant = re.compile(
            fr'(<plant[^>]*type\s*=\s*"{re.escape(plant_type)}"[^>]*>)(.*?)(</plant>)',
            re.DOTALL
        )
        matches = list(pattern_plant.finditer(events_content))
        if not matches:
            continue

        total_updates = [0]
        def replace_plant_block(match):
            header, inner, footer = match.groups()
            new_inner = inner
            for p_code in event_param_codes:
                if p_code in params.index:
                    val = params.loc[p_code, col]
                    if pd.notna(val) and str(val).strip().lower() not in ('nan', 'na', ''):
                        fval = float(val)
                        val_str = str(int(fval)) if fval == int(fval) else f"{fval:.6g}"
                        attr_pattern = re.compile(fr'({re.escape(p_code)}\s*=\s*")([^"]*)(")')
                        if attr_pattern.search(new_inner):
                            new_inner = attr_pattern.sub(fr'\g<1>{val_str}\g<3>', new_inner)
                            total_updates[0] += 1
            return header + new_inner + footer

        events_content = pattern_plant.sub(replace_plant_block, events_content)
        print(f"  -> Events XML Updated: {plant_type} from {col} ({total_updates[0]} attribute updates across {len(matches)} block(s))")

    with open(events_xml_path, 'w', encoding='utf-8') as f:
        f.write(events_content)
    print("SUCCESS: Events XML file synchronized.")
