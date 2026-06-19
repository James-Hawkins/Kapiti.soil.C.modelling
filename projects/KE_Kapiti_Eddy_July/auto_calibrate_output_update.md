# Objective
Update the `auto_calibrate.R` script to refine how output metrics are logged to `Calibr8.csv` and `state_new.csv`, strictly adhering to the user's specific formatting and logic requirements.

# Key Files & Context
- `KE_Kapiti_output_eddy/R_scripts/auto_calibrate.R`: The main calibration loop script.
- `KE_Kapiti_output_eddy/R_scripts/Calibr8.csv`: The configuration and iteration results log file.
- `KE_Kapiti_output_eddy/R_scripts/state_new.csv`: The longitudinal state tracking log file.

# Implementation Steps

1.  **Refine `Calibr8.csv` Output:**
    *   Update the logic writing to `Calibr8.csv` to ensure columns `old_value`, `new_value`, `nrmse_baseline`, `nrmse_new`, and `improvement_pct` are explicitly updated for the specific parameter row being evaluated.
    *   Update the `status` string to explicitly indicate the 1% threshold: `"KEPT (>= 1% improvement)"`, `"KEPT (< 1% improvement)"`, or `"REVERTED"`.

2.  **Targeted `state_new.csv` Logging:**
    *   Update the target state file path from `state.csv` to `state_new.csv`.
    *   Modify the initial load logic for `state_new.csv` to detect and filter out the pre-allocated comma-only rows (e.g., `,sks,bd...`) so they don't break the `rbind` operation or pollute the data frame.
    *   Refactor the `new_row` construction to **only** include the `iteration` column and the 15 specific parameter keys (e.g., `sks`, `bd`, `VCMAX_RED_OAT`, `VCMAX_TEPHROSIA`). It will drop the extraneous columns (`stage`, `nrmse_baseline`, etc.) that were previously logged in the state file.
    *   Ensure that the current parameter being evaluated is mapped to the correct column using the concatenated `PARAMETER_SPECIES` key, and all other 14 columns carry forward their previous values from the prior row.
    *   Write the updated state data frame back to `state_new.csv` omitting NA strings where possible to match the CSV format.

# Verification & Testing
- Run `Rscript auto_calibrate.R` for at least one iteration.
- Verify `Calibr8.csv` contains the correct values in columns J-N and the verbose status string in column O.
- Verify `state_new.csv` correctly appends exactly one new row containing the iteration number and the updated/carried-over parameter values across exactly 16 columns.