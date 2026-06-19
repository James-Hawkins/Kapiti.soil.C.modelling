import os
import subprocess
import csv
import xml.etree.ElementTree as ET

# Paths
CALIBR8_CSV = r'KE_Kapiti_output_eddy\R_scripts\Calibr8.csv'
METRICS_CSV = r'KE_Kapiti_output_eddy\R_scripts\metrics.csv'
STATE_CSV = r'KE_Kapiti_output_eddy\R_scripts\state.csv'
BAT_FILE = r'KE_Kapiti_eddy.bat'
VALID8_R = r'KE_Kapiti_output_eddy\valid8.R'

def get_nrmse():
    if not os.path.exists(METRICS_CSV):
        return float('inf')
    with open(METRICS_CSV, mode='r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            # We look for the first occurrence of r.a.swc.5.cm.osv
            if row['osv.variable'] == 'r.a.swc.5.cm.osv':
                try:
                    return float(row['nrmse'])
                except ValueError:
                    continue
    return float('inf')

def update_xml(xml_file, parameter, delta):
    # Check if file exists, if not, try with KE_Kapiti_ prefix
    if not os.path.exists(xml_file):
        alt_name = "KE_Kapiti_" + xml_file
        if os.path.exists(alt_name):
            xml_file = alt_name
        else:
            print(f"Error: Could not find XML file {xml_file} or {alt_name}")
            return None

    print(f"Updating {parameter} in {xml_file} with delta {delta}")
    tree = ET.parse(xml_file)
    root = tree.getroot()
    
    updated = False
    new_val = 0
    
    # Handle both siteparameters (par name=...) and site (layer sks=...)
    if 'siteparameters' in xml_file.lower():
        for par in root.findall(".//par"):
            if par.get('name') == parameter:
                current_val = float(par.get('value'))
                new_val = current_val + (current_val * delta)
                par.set('value', str(round(new_val, 5)))
                updated = True
    else:
        # Handle layer attributes like sks
        for layer in root.findall(".//layer"):
            if parameter in layer.attrib:
                current_val = float(layer.get(parameter))
                new_val = current_val + (current_val * delta)
                layer.set(parameter, str(round(new_val, 5)))
                updated = True
                
    if updated:
        tree.write(xml_file, encoding='utf-8', xml_declaration=True)
        print(f"New value: {new_val}")
        return new_val
    else:
        print(f"Parameter {parameter} not found in {xml_file}")
        return None

def run_simulation():
    print("Running simulation...")
    # Pipe an enter key to bypass the 'pause' in the batch file
    process = subprocess.Popen([BAT_FILE], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, shell=True)
    process.communicate(input='\n')
    print("Simulation complete.")

def run_validation():
    print("Running validation...")
    subprocess.run(['Rscript', VALID8_R], check=True, shell=True)
    print("Validation complete.")

def main():
    # 1. Baseline
    baseline_nrmse = get_nrmse()
    print(f"Baseline nRMSE: {baseline_nrmse}")
    
    # 2. Read Calibr8 config
    with open(CALIBR8_CSV, mode='r') as f:
        reader = csv.DictReader(f)
        config = next(reader) # Pick first parameter
    
    parameter = config['parameter']
    xml_file = config['file']
    delta = float(config['delta'])
    
    # 3. Update XML
    new_value = update_xml(xml_file, parameter, delta)
    if new_value is None:
        return

    # 4. Run Simulation
    run_simulation()
    
    # 5. Run Validation
    run_validation()
    
    # 6. Evaluate
    new_nrmse = get_nrmse()
    print(f"New nRMSE: {new_nrmse}")
    
    if new_nrmse < baseline_nrmse:
        print(f"Improvement found! Updating state.csv with {new_value}")
        # Update state.csv
        rows = []
        with open(STATE_CSV, mode='r') as f:
            reader = list(csv.reader(f))
            header = reader[0]
            for i, row in enumerate(reader[1:]):
                if i == 0: # Row 1
                    # Find column index
                    try:
                        col_idx = header.index(parameter)
                        row[col_idx] = str(round(new_value, 5))
                    except (ValueError, IndexError):
                        pass
                rows.append(row)
        
        with open(STATE_CSV, mode='w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(header)
            writer.writerows(rows)
    else:
        print("No improvement in nRMSE.")

if __name__ == "__main__":
    main()
