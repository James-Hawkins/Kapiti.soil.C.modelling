# Objective
Update the `auto_calibrate.R` script to support directional parameter updates, an auto-reversal retry loop, and multi-metric validation criteria (ensuring "other" metrics do not degrade).

# Key Files & Context
- `KE_Kapiti_output_eddy/R_scripts/auto_calibrate.R`: The main calibration loop script.
- `KE_Kapiti_output_eddy/R_scripts/Calibr8.csv`: The configuration file containing the new columns (`direction`, `nrmse_others_baseline`, `nrmse_others_new`, `status_others`).

# Implementation Steps

1.  **Directional Updates:**
    *   Update the `update_xml` function (or the logic calling it) to accept a `direction` parameter.
    *   If `direction` == "higher", `new_val = old_val * (1 + delta)`.
    *   If `direction` == "lower", `new_val = old_val * (1 - delta)`.

2.  **Other Metrics Tracking:**
    *   Implement a new function `get_other_nrmse(target_metrics)` that parses `metrics.csv`.
    *   It will identify all unique `osv.variable` names in the `all` period that are NOT in the `target_metrics` list.
    *   It will return the simple average of their nRMSE values.

3.  **Auto-Reversal & Acceptance Logic:**
    *   Wrap the evaluation logic for a single parameter row in a while loop or flag-based system that allows for exactly one retry.
    *   **Attempt 1:** Calculate baseline metrics (target and others). Apply the change based on the current `direction` in `Calibr8.csv`. Run simulation and validation.
    *   **Evaluation 1:**
        *   Did target nRMSE improve? AND Did "other" nRMSE *not* increase (`nrmse_others_new <= nrmse_others_baseline`)?
        *   If YES: Keep changes. Update `Calibr8.csv` with results and exit row evaluation.
        *   If NO (target degraded): Revert XML. Invert the direction string ("higher" -> "lower" or vice versa). Trigger Attempt 2.
    *   **Attempt 2 (Reversed Direction):** Apply the change using the new direction. Run simulation and validation.
    *   **Evaluation 2:**
        *   Apply the same dual criteria (target improves AND others do not degrade).
        *   If YES: Keep changes. Update `Calibr8.csv` with the *new* direction and results.
        *   If NO: Revert XML. Keep the *original* direction in `Calibr8.csv` (or record the failure).
    
4.  **Logging Updates:**
    *   Populate `nrmse_others_baseline` and `nrmse_others_new` in `Calibr8.csv`.
    *   Populate `status_others` with "INCREASED" or "NOT INCREASED".
    *   Ensure the main `status` column reflects the dual criteria success or failure.

# Verification & Testing
- Run `Rscript auto_calibrate.R`.
- Verify `Calibr8.csv` shows populated "other" metric columns.
- Confirm the script attempts reversed directions when the initial direction degrades the target nRMSE.
- Verify parameters are only kept if target improves AND `status_others` is "NOT INCREASED".