# Objective
Automate the iterative calibration sequence to optimize model parameters, replacing manual turn-by-turn execution with a robust script.

# Key Files & Context
- `KE_Kapiti_output_eddy/R_scripts/Calibr8.csv`: Source of the parameter to modify, the target XML file, and the step `delta`.
- Target XML file (e.g., `KE_Kapiti_site_eddy_Vertis_Gachene.xml`): File where the parameter will be updated.
- `KE_Kapiti_eddy.bat`: Batch script to run the LandscapeDNDC simulation.
- `KE_Kapiti_output_eddy/valid8.R`: R script to calculate validation metrics.
- `KE_Kapiti_output_eddy/R_scripts/metrics.csv` (or `KE_Kapiti_output_eddy/metrics.csv`): File containing the `nrmse` performance metrics.
- `KE_Kapiti_output_eddy/R_scripts/state.csv`: File to record successful parameter updates.

# Implementation Steps
1. **Create an Automation Script (`auto_calibrate.py`):**
   - Write a Python script in the root directory that acts as the orchestrator.
   - **Step 1: Baseline Metrics:** Read the initial `metrics.csv` to capture the baseline `nrmse` for the row where `osv.variable` is `r.a.swc.5.cm.osv` (we will target the first occurrence or the `all` period).
   - **Step 2: Read Calibration Config:** Read `Calibr8.csv` to extract the `parameter` (e.g., `sks`), `file` (e.g., `site_eddy_Vertis_Gachene.xml`), and `delta` (e.g., `0.1`).
   - **Step 3: Modify XML:** Parse the target XML file, locate the specified parameter, compute `new_value = current_value + (current_value * delta)`, and overwrite the XML file.
   - **Step 4: Run Simulation:** Execute `KE_Kapiti_eddy.bat` using a subprocess call that pipes an "Enter" keystroke (`echo. |`) to automatically bypass the trailing `pause` command.
   - **Step 5: Run Validation:** Execute `Rscript KE_Kapiti_output_eddy/valid8.R` to generate the updated `metrics.csv`.
   - **Step 6: Evaluate & Record:** Read the newly generated `metrics.csv`. If the new `nrmse` is lower than the baseline, update `KE_Kapiti_output_eddy/R_scripts/state.csv` by inserting `new_value` into the column matching the parameter name in row 1.
2. **Execution Phase:**
   - Once the script is created, run `python auto_calibrate.py` to perform the automated sequence.

# Verification & Testing
- Run the script and verify that the XML file is updated with the correct calculated `new_value`.
- Verify that the `.bat` and `.R` scripts run without hanging.
- Verify that `state.csv` is updated correctly if and only if the `nrmse` improves.
