import re
import os

file_path = r'..\KE_Kapiti_speciesparameters_GLOBAL.xml'

new_params = {
    'KO25': '375',
    'KM20': '0.020',
    'VCMAX': '40.0',
    'AEJM': '57450',
    'AEKC': '57865',
    'AEKO': '45850',
    'AERD': '54900',
    'AEVC': '71715',
    'AEVO': '38445',
    'KC25': '533.8',
    'THETA': '0.815',
    'QJVC': '3.475',
    'QRD25': '0.03',
    'WUECMAX': '6.38',
    'WUECMIN': '2.60',
    'H2OREF_A': '0.45',
    'H2OREF_SENESCENCE': '0.2275',
    'H2OREF_GS': '0.24',
    'FRACTION_FOLIAGE': '0.275',
    'FRACTION_FRUIT': '0.05',
    'FRACTION_ROOT': '0.675',
    'MFOLOPT': '0.275',
    'MWFM': '0.000275',
    'SLAMAX': '22.5',
    'SLAMIN': '13.25',
    'AMX25': '27.5',
    'GDD_BASE_TEMPERATURE': '12.5',
    'GDD_EMERG': '80.0'
}

# Also update these if they exist, to keep consistency
synonyms = {
    'AEV0': '38445',
    'GDD_EMERGENCE': '80.0',
    'VCMAX25': '40.0'
}

with open(file_path, 'r') as f:
    lines = f.readlines()

new_lines = []
in_species_block = False
in_comment = False
updated_in_this_block = set()

for line in lines:
    # Track comments
    if '<!--' in line:
        # Check if it also ends on the same line
        if '-->' in line:
            pass # No change in in_comment state
        else:
            in_comment = True
    elif '-->' in line:
        in_comment = False
        new_lines.append(line)
        continue

    # Track species block
    if '<species' in line and 'mnemonic="RED_OAT"' in line and 'name="RED_OAT"' in line:
        in_species_block = True
        updated_in_this_block = set()
        new_lines.append(line)
        continue
    
    if in_species_block:
        if '</species>' in line:
            # Add missing parameters
            for name, value in new_params.items():
                if name not in updated_in_this_block:
                    new_lines.append(f'      <par name="{name}" value="{value}"/>\n')
            in_species_block = False
            new_lines.append(line)
            continue
        
        if not in_comment:
            updated = False
            # Check for exact matches
            for name, value in new_params.items():
                if f'name="{name}"' in line:
                    line = re.sub(r'value="[^"]*"', f'value="{value}"', line)
                    updated_in_this_block.add(name)
                    updated = True
                    break
            
            if not updated:
                # Check for synonyms
                for name, value in synonyms.items():
                    if f'name="{name}"' in line:
                        line = re.sub(r'value="[^"]*"', f'value="{value}"', line)
                        updated = True
                        break
            
            new_lines.append(line)
        else:
            # Inside comment, don't update
            new_lines.append(line)
    else:
        new_lines.append(line)

with open(file_path, 'w') as f:
    f.writelines(new_lines)
print("Successfully updated RED_OAT block with comment protection.")
