import openpyxl
import pandas as pd
import re

source_xlsm = 'Species_params_GEMINI.xlsm'
lres_master_path = 'Kapiti_LDNDC_data_Master.xlsx'
target_xlsm = 'Weighted_species_params_master.xlsm'

PARENT_SHEETS = {
    'cynodon_dactylon': 'Lresources_PAVI',
    'cenchrus_mezianum': 'Lresources_PERG',
    'themeda_triandra': 'Lresources_PERG',
    'indigofera_volkensii': 'Lresources_ALFA',
    'vachellia_drepanolobium': 'Lresources_ACTO'
}

print(f"Loading L-DNDC parent species library from {lres_master_path}...")
wb_lres = openpyxl.load_workbook(lres_master_path, read_only=True)
lres_lookup = {}
for col_name, sh_name in PARENT_SHEETS.items():
    lres_lookup[col_name] = {}
    if sh_name in wb_lres.sheetnames:
        ws_lres = wb_lres[sh_name]
        for row in ws_lres.iter_rows(min_col=1, max_col=1, values_only=True):
            val = row[0]
            if val and isinstance(val, str) and '.parameter.' in val:
                m = re.search(r'\.parameter\.([A-Za-z0-9_]+)\.value\s*=\s*"([^"]+)"', val)
                if m:
                    lres_lookup[col_name][m.group(1).upper()] = m.group(2)
wb_lres.close()

print(f"Loading source {source_xlsm}...")
wb_src = openpyxl.load_workbook(source_xlsm)
ws_src = wb_src.active

header_row = [cell.value for cell in ws_src[1]]
header_idx = {val: idx for idx, val in enumerate(header_row, start=1) if val is not None}

sci_col = header_idx['scientific_name']
code_col = header_idx['code']
midpoint_col = header_idx['midpoint']

def resolve_value(ws, row, col):
    val = ws.cell(row=row, column=col).value
    visited = set()
    while isinstance(val, str) and val.startswith('='):
        ref_cell = val[1:]
        if ref_cell in visited:
            break
        visited.add(ref_cell)
        try:
            val = ws[ref_cell].value
        except:
            break
    return val

vachellia_params = {}
for r in range(2, ws_src.max_row + 1):
    sci_val = resolve_value(ws_src, r, sci_col)
    code_val = resolve_value(ws_src, r, code_col)
    if sci_val is None or code_val is None:
        continue
    sci = str(sci_val).strip().lower()
    code = str(code_val).strip().upper()
    if 'vachellia' in sci or 'drepallobium' in sci:
        midpoint = resolve_value(ws_src, r, midpoint_col)
        if pd.notna(midpoint) and str(midpoint).strip() not in ('', 'None', 'nan', 'NA'):
            try:
                vachellia_params[code] = float(midpoint)
                if vachellia_params[code] == int(vachellia_params[code]):
                    vachellia_params[code] = int(vachellia_params[code])
            except:
                vachellia_params[code] = midpoint

def get_lres_val(col_name, code):
    sp_dict = lres_lookup.get(col_name, {})
    if code in sp_dict:
        return sp_dict[code]
    if code == 'AEVO' and 'AEV0' in sp_dict:
        return sp_dict['AEV0']
    if code == 'AEV0' and 'AEVO' in sp_dict:
        return sp_dict['AEVO']
    if code == 'VCMAX25' and 'VCMAX' in sp_dict:
        return sp_dict['VCMAX']
    if code == 'VCMAX' and 'VCMAX25' in sp_dict:
        return sp_dict['VCMAX25']
    if code == 'GDD_EMERG' and 'GDD_EMERGENCE' in sp_dict:
        return sp_dict['GDD_EMERGENCE']
    return None

print(f"Updating {target_xlsm}...")
wb_tgt = openpyxl.load_workbook(target_xlsm, keep_vba=True)
ws_tgt = wb_tgt.active

headers_tgt = [cell.value for cell in ws_tgt[1]]
col_indices = {col_name: headers_tgt.index(col_name) + 1 for col_name in PARENT_SHEETS if col_name in headers_tgt}

skip_codes = {'FRACTIONALCOVER', 'WEIGHT.ALL', 'WEIGHT.FT', 'BIOMASS.YIELD.MG.HA.YR', 'INITIALBIOMASS', 'HEIGHTMAX', 'ROOTINGDEPTH'}

for r in range(5, ws_tgt.max_row + 1):
    code_cell = ws_tgt.cell(row=r, column=1).value
    if not code_cell:
        continue
    code = str(code_cell).strip().upper()
    if code in skip_codes:
        continue
    
    # 1. Populate vachellia_drepanolobium from literature first, then Lresources_ACTO fallback
    q_col = col_indices['vachellia_drepanolobium']
    val = vachellia_params.get(code)
    if val is None and code == 'VCMAX25':
        val = vachellia_params.get('VCMAX')
    elif val is None and code == 'VCMAX':
        val = vachellia_params.get('VCMAX25')
    elif val is None and code == 'AEVO':
        val = vachellia_params.get('AEV0')
    
    source_tag = "LIT"
    if val is None:
        raw_fb = get_lres_val('vachellia_drepanolobium', code)
        if raw_fb is not None:
            try:
                f_fb = float(raw_fb)
                val = int(f_fb) if f_fb == int(f_fb) else f_fb
            except:
                val = raw_fb
            source_tag = "Lresources_ACTO"
            
    if val is not None:
        ws_tgt.cell(row=r, column=q_col).value = val
        print(f"  Row {r:2d} ({code:22s}) [vachellia_drepanolobium]: {val} ({source_tag})")
        
    # 2. Check other species columns for any missing (None) cells and fill from their respective Lresources sheet
    for sp_col, c_idx in col_indices.items():
        if sp_col == 'vachellia_drepanolobium':
            continue
        curr_val = ws_tgt.cell(row=r, column=c_idx).value
        if curr_val is None or str(curr_val).strip() in ('', 'None', 'nan', 'NA'):
            raw_fb = get_lres_val(sp_col, code)
            if raw_fb is not None:
                try:
                    f_fb = float(raw_fb)
                    fb_val = int(f_fb) if f_fb == int(f_fb) else f_fb
                except:
                    fb_val = raw_fb
                ws_tgt.cell(row=r, column=c_idx).value = fb_val
                print(f"  Row {r:2d} ({code:22s}) [{sp_col}]: {fb_val} ({PARENT_SHEETS[sp_col]})")

try:
    wb_tgt.save(target_xlsm)
    print(f"Successfully updated {target_xlsm}.")
except PermissionError:
    temp_xlsm = 'Weighted_species_params_master_temp.xlsm'
    wb_tgt.save(temp_xlsm)
    print(f"Note: {target_xlsm} is currently open in Excel. Saved updated workbook to {temp_xlsm}.")
