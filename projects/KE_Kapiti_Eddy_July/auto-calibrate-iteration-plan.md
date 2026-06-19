# Objective
Enhance the iterative calibration script to perform continuous cycling over the parameters defined in `Calibr8.csv` and propagate the successful state across multiple iterations in `state.csv`.

# Scope & Context
- Target script: `auto_calibrate.R`
- Target state file: `KE_Kapiti_output_eddy/R_scripts/state.csv` (e.g., 5 rows).
- Config file: `KE_Kapiti_output_eddy/R_scripts/Calibr8.csv` (e.g., 2 rows).

# Implementation Steps (Continuous Loop)
1. **Determine Iterations:** Count the number of rows in `state.csv` (e.g., 5).
2. **Determine Parameters:** Count the number of rows in `Calibr8.csv` (e.g., `P`).
3. **Loop:** For `iteration` from 1 to 5:
   - **State Propagation:** If `iteration > 1`, copy the entire row `iteration - 1` to row `iteration` in `state.csv`. This ensures that successful parameters from previous rounds are carried forward.
   - **Parameter Selection:** Pick the parameter using modulo arithmetic: `param_idx = ((iteration - 1) %% P) + 1`. Extract the `delta`, `parameter`, and `file`.
   - **Baseline Metrics:** Open and read `metrics.csv`. Store the `nrmse` for `r.a.swc.5.cm.osv` into memory.
   - **Modify Parameter:** Change the parameter in the XML file by the `delta` (`new_value = old_value + old_value * delta`). **Backup** the XML file before modification.
   - **Run Simulation:** Execute `KE_Kapiti_eddy.bat`.
   - **Run Validation:** Execute `valid8.R`.
   - **Evaluate and Record:** 
     - Read the new `nrmse` from `metrics.csv`.
     - If the `nrmse` is **lower** than the baseline: insert `new_value` into the correct parameter column for the current `iteration` row in `state.csv`.
     - **Otherwise**: insert the `old_value` into the current `iteration` row in `state.csv`, and **revert** the XML file from the backup.

# Verification
1. Run `Rscript auto_calibrate.R`.
2. The output should show Iteration 1 (sks), Iteration 2 (vangenuchten_n), Iteration 3 (sks), Iteration 4 (vangenuchten_n), etc.
3. `state.csv` should show cumulative progress without `NA` gaps in subsequent rows.
