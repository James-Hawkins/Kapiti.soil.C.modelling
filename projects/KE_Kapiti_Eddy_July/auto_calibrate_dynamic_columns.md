# Objective
Update `auto_calibrate.R` to strictly synchronize the columns of `state_new.csv` with the active parameters in `Calibr8.csv` and refine the species parameter naming convention.

# Key Files & Context
- `KE_Kapiti_output_eddy/R_scripts/auto_calibrate.R`: The main calibration script.
- `KE_Kapiti_output_eddy/R_scripts/Calibr8.csv`: The active configuration defining parameters.
- `KE_Kapiti_output_eddy/R_scripts/state_new.csv`: The longitudinal state log.

# Implementation Steps

1.  **Refine Parameter Naming Logic:**
    *   Modify `get_param_key` to accept `p` (parameter), `s` (species), and `f` (xml_file).
    *   The logic will evaluate `str_detect(f, "speciesparameters_GLOBAL")`. Only if this is true, AND a valid species is provided, will it return `paste0(p, "_", s)`.
    *   Otherwise, it will return just `p`.

2.  **Generate Definitive Column List:**
    *   Right after loading `Calibr8.csv`, loop through its rows using the updated `get_param_key` function to build a definitive character vector of `all_param_keys`.
    *   Ensure this list represents the exact horizontal layout expected for the current run.

3.  **Dynamic State DataFrame Synchronization:**
    *   When loading `state_new.csv` from disk, define the `target_cols` as `c("iteration", all_param_keys)`.
    *   Identify any columns in `target_cols` missing from the loaded `state` and add them (initialized to NA).
    *   Force the `state` dataframe to contain **only** the `target_cols` in the exact order specified. This handles both the addition of new columns and the pruning of obsolete ones.
    *   When a new row is appended at the end of an iteration, enforce the same strict `target_cols` layout before writing to disk.

# Verification
- Add a new parameter to `Calibr8.csv` or remove an existing one.
- Run `Rscript auto_calibrate.R`.
- Verify the header row of `state_new.csv` perfectly matches the expected `iteration` + active `Calibr8.csv` keys.
- Verify parameters originating from files other than `speciesparameters_GLOBAL.xml` do not get species suffixes, even if a species column inadvertently contains data in the CSV.