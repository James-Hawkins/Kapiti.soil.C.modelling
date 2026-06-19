import pandas as pd
import re

# Paths according to parent-child workflow
child_path = 'Weighted_species_params_child.xlsx'
xml_path = '../KE_Kapiti_speciesparameters_GLOBAL.xml'

# Load data from CHILD
df = pd.read_excel(child_path)
params = df.set_index('code')

# Mnemonic to Excel column mapping
target_mappings = {
    'GRASS.HYBRID.1': 'HYBRID.GRASS',
    'FORB.HYBRID.1': 'HYBRID.FORB',
    'HYBRID_SHRUB': 'HYBRID.SHRUB',
    'RED_OAT': 'themeda_triandra',
    'INDIGO': 'indigofera_volkensii',
    'WHISTL_THORN_manual': 'vechellia_drepanolobium',
    'WHISTL_THORN2': 'vechellia_drepanolobium',
    'HYBRID.ALL': 'HYBRID.ALL'
}

# Parameter code mapping (Excel -> XML par name)
# Note: Excel code is the KEY, XML par name is the VALUE
mapping = {
    'AEJM': 'AEJM',
    'AEKC': 'AEKC',
    'AEKO': 'AEKO',
    'AERD': 'AERD',
    'AEVC': 'AEVC',
    'AEVO': 'AEV0',
    'KC25': 'KC25',
    'KM20': 'KM20',
    'VCMAX25': 'VCMAX25',
    'THETA': 'THETA',
    'GSMAX': 'GSMAX',
    'GSMIN': 'GSMIN',
    'WUECMAX': 'WUECMAX',
    'WUECMIN': 'WUECMIN',
    'H2OREF_A': 'H2OREF_A',
    'H2OREF_SENESCENCE': 'H2OREF_SENESCENCE',
    'H2OREF_GS': 'H2OREF_GS',
    'H2OREF_FLUSHING': 'H2OREF_FLUSHING',
    'H2OREF_LEAF_GROWTH': 'H2OREF_LEAF_GROWTH',
    'GDD_BASE_TEMPERATURE': 'GDD_BASE_TEMPERATURE',
    'GDD_EMERG': 'GDD_EMERGENCE',
    'GDD_MATURITY': 'GDD_MATURITY',
    'GDD_STEM_ELONGATION': 'GDD_STEM_ELONGATION',
    'GDD_ROOTS_GROWN': 'GDD_ROOTS_GROWN',
    'GDDFOLSTART': 'GDDFOLSTART',
    'GDDFOLEND': 'GDDFOLEND',
    'NDFLUSH': 'NDFLUSH',
    'NDMORTA': 'NDMORTA',
    'FRACTION_ROOT': 'FRACTION_ROOT',
    'FRACTION_FOLIAGE': 'FRACTION_FOLIAGE',
    'FRACTION_FRUIT': 'FRACTION_FRUIT',
    'MFOLOPT': 'MFOLOPT',
    'MWFM': 'MWFM',
    'SLAMAX': 'SLAMAX',
    'SLAMIN': 'SLAMIN',
    'KO25': 'KO25'
}

with open(xml_path, 'r') as f:
    xml_content = f.read()

print("Synchronizing XML with Child data (Rule: Skipping NaNs)...")
for mnemonic, col in target_mappings.items():
    if col not in df.columns:
        print(f"  Warning: Excel column '{col}' not found. Skipping {mnemonic}.")
        continue
    
    # Regex to find the specific species block
    pattern_block = fr'(<species[^>]*mnemonic="{mnemonic}"[^>]*>)(.*?)(</species>)'
    match = re.search(pattern_block, xml_content, re.DOTALL)
    
    if not match:
        # Try a more flexible match if mnemonic spacing is weird
        pattern_block = fr'(<species[^>]*mnemonic\s*=\s*"{mnemonic}"[^>]*>)(.*?)(</species>)'
        match = re.search(pattern_block, xml_content, re.DOTALL)

    if not match:
        print(f"  Warning: XML mnemonic '{mnemonic}' not found.")
        continue
    
    header, body, footer = match.groups()
    new_body = body
    update_count = 0
    
    for excel_code, xml_name in mapping.items():
        if excel_code in params.index:
            val = params.loc[excel_code, col]
            
            # CRITICAL: Only update if value is not NaN
            if pd.notna(val):
                # Format value string
                if val == int(val):
                    val_str = str(int(val))
                else:
                    val_str = f"{val:.6g}"
                
                # Pattern to update existing par (handles both single line and split tag)
                par_pattern = fr'(<par name="{xml_name}" value=")([^"]*)("(?:\s*/?>|\s*></par>))'
                
                if re.search(par_pattern, new_body):
                    new_body = re.sub(par_pattern, fr'\g<1>{val_str}\g<3>', new_body)
                    update_count += 1
                # (Optional: if we wanted to ADD missing pars, we would do it here, 
                # but user requested sync which usually means updating existing ones).

    # Reconstruct block and update main content
    xml_content = xml_content.replace(match.group(0), header + new_body + footer)
    print(f"  -> {mnemonic} synced from {col} ({update_count} parameters updated).")

# Write back to XML
with open(xml_path, 'w') as f:
    f.write(xml_content)

print("\nSUCCESS: XML file synchronized with master data (NaNs preserved).")
