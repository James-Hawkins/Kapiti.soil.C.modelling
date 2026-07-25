import os
import sys
import win32com.client
import xml.etree.ElementTree as ET
import re

# Mapping from Calibr8 names to Excel column names
SPECIES_MAP = {
    'RED_OAT': 'themeda_triandra',
    'CYNODON': 'cynodon_dactylon',
    'CENCHRUS': 'cenchrus_mezianum',
    'INDIGOFERA': 'indigofera',
    'TEPHROSIA': 'tephrosia'
}

TARGET_SPECIES = ['cynodon_dactylon', 'cenchrus_mezianum', 'themeda_triandra', 'indigofera', 'tephrosia']

def update_xml_parameters(xml_path, species_name, params, is_event=False):
    if not os.path.exists(xml_path):
        print(f"Warning: {xml_path} not found.")
        return

    try:
        with open(xml_path, 'r', encoding='utf-8') as f:
            content = f.read()

        changed = False
        if not is_event:
            # speciesparameters_GLOBAL.xml: <species name="..."> ... </species>
            pattern = re.compile(r'(<species[^>]*name="' + re.escape(species_name) + r'"[^>]*>)(.*?)(</species>)', re.DOTALL)
            match = pattern.search(content)
            if match:
                header, inner, footer = match.groups()
                new_inner = inner
                
                # Synonyms mapping
                synonyms = {'VCMAX': ['VCMAX', 'VCMAX25'], 'AEVO': ['AEVO', 'AEV0']}
                
                updated_params = set()
                
                for p_name, p_val in params.items():
                    val_str = str(round(p_val, 6))
                    targets = synonyms.get(p_name, [p_name])
                    
                    found = False
                    for target in targets:
                        # Even simpler regex
                        p_pattern = re.compile(r'(?P<pre><par[^>]+name="' + re.escape(target) + r'"[^>]+value=")(?P<val>[^"]+)(?P<post>")')
                        if p_pattern.search(new_inner):
                            # Use lambda to avoid backreference parsing in replacement string
                            new_inner = p_pattern.sub(lambda m: m.group('pre') + val_str + m.group('post'), new_inner)
                            updated_params.add(p_name)
                            found = True
                            break

                    if not found and p_name not in ('id', 'code', 'category', 'include.xml', 'xml.file'):
                        # Add missing parameter before closing tag
                        new_inner = new_inner.strip() + f'\n      <par name="{p_name}" value="{val_str}"/>\n    '
                        updated_params.add(p_name)

                if new_inner != inner:
                    content = content[:match.start()] + header + new_inner + footer + content[match.end():]
                    changed = True
        else:
            # events_eddy.xml: <plant type="..."> ... </plant>
            pattern = re.compile(r'(<plant[^>]*type="' + re.escape(species_name) + r'"[^>]*>)(.*?)(</plant>)', re.DOTALL)
            
            def replace_plant(match):
                nonlocal changed
                header, inner, footer = match.groups()
                new_header = header
                new_inner = inner
                
                for p_name, p_val in params.items():
                    val_str = str(round(p_val, 6))
                    # Check in header (<plant ... >) and inner (child tags like <crop ... />)
                    # Use named groups and lambda to avoid backreference issues
                    p_pattern = re.compile(r'(?P<pre>' + re.escape(p_name) + r'\s*=\s*")(?P<val>[^"]+)(?P<post>")')
                    
                    if p_pattern.search(new_header):
                        new_header = p_pattern.sub(lambda m: m.group('pre') + val_str + m.group('post'), new_header)
                    if p_pattern.search(new_inner):
                        new_inner = p_pattern.sub(lambda m: m.group('pre') + val_str + m.group('post'), new_inner)
                
                if new_header != header or new_inner != inner:
                    changed = True
                return new_header + new_inner + footer

            new_content = pattern.sub(replace_plant, content)
            if changed:
                content = new_content
        
        if changed:
            with open(xml_path, 'w', encoding='utf-8') as f:
                f.write(content)
            print(f"Updated {xml_path}")
            
    except Exception as e:
        print(f"Error updating XML {xml_path}: {e}")

def main():
    if len(sys.argv) < 2:
        print("Usage: python update_veg_mix.py <command> [species] [direction] [delta] [pmin] [pmax]")
        print("Commands: read, update, sync")
        return

    command = sys.argv[1]
    
    excel_path = os.path.abspath('data/Weighted_species_params_master.xlsm')
    
    excel = win32com.client.Dispatch("Excel.Application")
    excel.Visible = False
    excel.DisplayAlerts = False
    
    try:
        wb = excel.Workbooks.Open(excel_path)
        ws = wb.ActiveSheet
        
        # 1. Map columns and rows
        headers = {}
        for c in range(1, 30):
            val = ws.Cells(1, c).Value
            if val: headers[str(val).strip()] = c
            
        row_names = {}
        for r in range(1, 50):
            val = ws.Cells(r, 1).Value
            if val: row_names[str(val).strip()] = r
            
        if command in ("read", "update"):
            species_calibr8 = sys.argv[2]
            mapped_species = SPECIES_MAP.get(species_calibr8, species_calibr8)
            
            frac_row = row_names.get('fractionalcover')
            if not frac_row:
                print("ERROR: Row 'fractionalcover' not found.")
                return

            target_col = headers.get(mapped_species)
            if not target_col:
                print(f"ERROR: Species column '{mapped_species}' not found.")
                return

            current_weight = float(ws.Cells(frac_row, target_col).Value or 0.0)
            
            if command == "read":
                print(f"RESULT: {current_weight}")
                wb.Close(False)
                return

            if command == "update":
                direction = sys.argv[3]
                delta = float(sys.argv[4])
                pmin = float(sys.argv[5]) if sys.argv[5] != "NA" else None
                pmax = float(sys.argv[6]) if sys.argv[6] != "NA" else None
                
                delta_mult = (1 + delta) if direction == "higher" else (1 - delta)
                new_weight = current_weight * delta_mult
                
                hit_bound = False
                if pmin is not None and new_weight < pmin:
                    new_weight = pmin
                    hit_bound = True
                if pmax is not None and new_weight > pmax:
                    new_weight = pmax
                    hit_bound = True
                    
                if new_weight == current_weight:
                    print(f"RESULT: {current_weight}, {new_weight}, {hit_bound}")
                    wb.Close(False)
                    return
                    
                change = new_weight - current_weight
                
                # Distribute change to others
                others = [s for s in TARGET_SPECIES if SPECIES_MAP.get(s, s) != mapped_species]
                other_data = []
                total_other_weight = 0.0
                for s in others:
                    col = headers.get(s)
                    if col:
                        val = float(ws.Cells(frac_row, col).Value or 0.0)
                        other_data.append((col, val))
                        total_other_weight += val
                
                ws.Cells(frac_row, target_col).Value = new_weight
                for col, val in other_data:
                    if total_other_weight > 0:
                        s_change = -change * (val / total_other_weight)
                    else:
                        s_change = -change / len(others)
                    ws.Cells(frac_row, col).Value = max(0.0, val + s_change)
                    
                excel.Calculate()
                wb.Save()

        # Commmon part for sync and update: Read HYBRID columns and Update XMLs
        if command in ("update", "sync"):
            # Map Excel column names to XML species names
            HYBRID_MAP = {
                'HYBRID.ALL': 'HYBRID.ALL',
                'HYBRID.GRASS': 'GRASS.HYBRID.1',
                'HYBRID.FORB': 'FORB.HYBRID.1'
            }
            
            include_col = headers.get('include.xml', 3)
            category_col = headers.get('category', 2)
            
            for excel_col_name, xml_species_name in HYBRID_MAP.items():
                col_idx = headers.get(excel_col_name)
                if not col_idx:
                    print(f"Warning: Column {excel_col_name} not found in Excel.")
                    continue
                
                species_params = {}
                event_params = {}
                
                # Rows 4 to 50
                for r in range(4, 51):
                    param_code = ws.Cells(r, 1).Value
                    if not param_code: continue
                    
                    inc = ws.Cells(r, include_col).Value
                    cat = ws.Cells(r, category_col).Value
                    val = ws.Cells(r, col_idx).Value
                    
                    if str(inc).strip().upper() == "TRUE" and val is not None:
                        param_name = str(param_code).strip()
                        if str(cat).strip().lower() == "species":
                            species_params[param_name] = float(val)
                        elif str(cat).strip().lower() == "event" or param_name in ('heightmax', 'rootingdepth', 'initialbiomass'):
                            event_params[param_name] = float(val)

                # Update XMLs for this specific hybrid species
                update_xml_parameters('KE_Kapiti_speciesparameters_GLOBAL.xml', xml_species_name, species_params, is_event=False)
                update_xml_parameters('KE_Kapiti_events_eddy.xml', xml_species_name, event_params, is_event=True)

            wb.Close()
            
            if command == "update":
                print(f"RESULT: {current_weight}, {new_weight}, {hit_bound}")
            else:
                print("RESULT: Sync complete")

    except Exception as e:
        print(f"ERROR: {e}")
    finally:
        excel.Quit()

if __name__ == "__main__":
    main()
