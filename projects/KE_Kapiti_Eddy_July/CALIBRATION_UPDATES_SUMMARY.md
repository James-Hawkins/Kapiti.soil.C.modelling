# Calibration Procedure Updates Summary

During this session, the `auto_calibrate.R` script was significantly enhanced to support a more robust and automated calibration workflow.

## Key Enhancements

### 1. Robust Output & State Logging
- **Calibr8.csv**: Now explicitly populates columns J-S with `old_value`, `new_value`, `nrmse_baseline`, `nrmse_new`, `nrmse_others_baseline`, `nrmse_others_new`, `improvement_pct`, `status`, and `status_others`.
- **Status Thresholds**: The `status` column now specifies if an improvement was `>= 1%` or `< 1%`.
- **Dynamic State Synchronization**: `state_new.csv` columns are now dynamically synchronized with the active parameters in `Calibr8.csv`. Obsolete columns are pruned, and new ones are added automatically.

### 2. Baseline Management
- **Persistent Baselines**: At the start of every run, the script identifies all unique XML files and creates `*_baseline.xml` versions if they don't exist.
- **Automatic Reset**: The script forces a reset of all working XML files to their baseline state at the beginning of iteration 1, ensuring a consistent starting point.

### 3. Directional Search & Auto-Reversal
- **Directional Deltas**: Supports `higher` and `lower` search directions as defined in `Calibr8.csv`.
- **Auto-Reversal Loop**: If a parameter update in the specified direction degrades the target nRMSE, the script automatically reverts the change, inverts the direction, and retries the iteration.

### 4. Multi-Metric Validation
- **Secondary Metric Guard**: Added a "Others" nRMSE check. A parameter change is only kept if:
  1. The target validation metrics improve.
  2. The average nRMSE of all other metrics does *not* increase.
- **`status_others`**: Reports "INCREASED" or "NOT INCREASED" for the non-target variables.

### 5. Species-Specific Parameter Mapping
- **Strict Naming**: Species suffixes (e.g., `_TEPHROSIA`) are now only applied to parameters defined within `KE_Kapiti_speciesparameters_GLOBAL.xml`. 
- **Full Initialization**: Every row in `state_new.csv` is now fully populated from Iteration 1 by pre-loading initial values from the baseline XMLs.

### 6. Convergence Auto-Skip
- **Efficiency Logic**: If a parameter evaluation results in an improvement of `< 1%`, a `REVERTED` status, or `BOUND_REACHED`, the script now automatically sets `include <- FALSE` for that row in `Calibr8.csv`.
- **Skip Logging**: Future iterations will skip these parameters, logging "Skipping [Parameter]: 'include' is FALSE."

## Bug Fixes & Stability
- **XML Parsing**: Improved regex patterns to handle multiple parameter styles and ensure correct capturing of values.
- **Error Handling**: Implemented `try-error` blocks around XML updates and CSV writes to prevent crashes due to locked files (e.g., if open in Excel) or missing species blocks.
- **File Integrity**: Added backup/revert logic for all XML modifications to ensure the workspace remains clean after failed iterations.
