# Objective
Update `auto_calibrate.R` to establish a persistent baseline for all XML files and to initialize the state tracking so all parameter columns are fully populated from Iteration 1.

# Key Files & Context
- `KE_Kapiti_output_eddy/R_scripts/auto_calibrate.R`: The main calibration script.
- `KE_Kapiti_output_eddy/R_scripts/Calibr8.csv`: Source of required files and parameters.

# Implementation Steps

1.  **Baseline Initialization & Reset:**
    *   Identify unique target XML files from `Calibr8.csv`.
    *   Resolve their full paths (checking for `.xml` or `KE_Kapiti_` prefixes).
    *   For each resolved file (e.g., `KE_Kapiti_siteparameters.xml`), define a baseline path (e.g., `KE_Kapiti_siteparameters_baseline.xml`).
    *   If the baseline does not exist, copy the current file to create it.
    *   Force a reset: Copy all baseline files back over the active files to guarantee iteration 1 always starts from the baseline state.

2.  **Read XML Parameter Values:**
    *   Create a helper function `read_xml_value(xml_file, parameter, species_name = NA)` leveraging the same regex parsing logic used in `update_xml` but returning the captured `old_val` without making any modifications.

3.  **Full State Pre-population:**
    *   Before the iteration loop starts, create a named list or environment to hold the "current_parameter_state".
    *   Loop through every row in `Calibr8.csv`. For each parameter key (e.g., `sks`, `VCMAX_TEPHROSIA`), call `read_xml_value` against the *baseline* XML to get its true starting value.
    *   Populate `current_parameter_state` with these values.

4.  **State Logging Updates:**
    *   Inside the calibration loop, when an iteration finishes and writes to `state_new.csv`, it will pull the values for all 15 columns from `current_parameter_state`.
    *   If a parameter was successfully calibrated in the current iteration, update `current_parameter_state` with the `new_val` before appending the row to `state_new.csv`.
    *   This guarantees columns B to Q never contain NA/blank values due to the parameter not having been evaluated yet.

# Verification
- Run the script and abort after 2-3 iterations.
- Verify `*_baseline.xml` files exist in the project root.
- Inspect `state_new.csv` to ensure row 1 (and all subsequent rows) have numbers in all 15 parameter columns, matching the baseline XMLs.