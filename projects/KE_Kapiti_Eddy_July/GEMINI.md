# Project: Kapiti Soil C Modelling (KE_Kapiti_Eddy_July)

## Project Overview
This project focuses on modelling soil carbon and water dynamics at the Kapiti site using the **LandscapeDNDC** (L-DNDC) model. The workflow involves running simulations via batch files and validating the results using R scripts against observed Eddy Covariance and biometric data.

## Automated Calibration Workflow
An automated calibration system has been implemented to optimize model parameters (e.g., `sks`, `vangenuchten_n`) based on the Normalized Root Mean Square Error (nRMSE) of soil water content (`r.a.swc.5.cm.osv`).

### Key Automation Script: `auto_calibrate.R`
This script performs an iterative optimization loop:
1.  **Continuous Cycling**: Iterates through parameters defined in `KE_Kapiti_output_eddy/R_scripts/Calibr8.csv`.
2.  **Targeted Soil Calibration**: Supports layer-specific updates for parameters with `class = "soil"`. It uses the `soil.layer` column (e.g., "5 cm") to target specific `<layer>` tags in the XML (e.g., `depth="50"`).
3.  **Vegetation Mix Optimization (`veg.mix`)**: Supports a complex calibration class where adjusting a species' weight triggers a proportional redistribution across other constituent species in `Weighted_species_params.xlsx`. This process automatically recalculates and syncs the `HYBRID_ALL` parameters into both `speciesparameters_GLOBAL.xml` and `events_eddy.xml`.
4.  **State Propagation**: Carries forward successful parameter updates across iterations, logging them row-by-row in `KE_Kapiti_output_eddy/R_scripts/state_new.csv`. Layer-specific parameters are tracked as distinct columns (e.g., `sks_5_cm`).
4.  **Automatic Fallback**: Backs up XML files before modification. If an iteration does not improve the `nrmse`, it reverts the XML file and records the original value in the state log.
5.  **Batch Integration**: Automatically handles the simulation (`KE_Kapiti_eddy.bat`) and validation (`valid8.R`) phases.

### How to Run Calibration
To perform a calibration run, ensure `Calibr8.csv` contains your target parameters and run:
```powershell
& "C:\Program Files\R\R-4.5.0\bin\x64\Rscript.exe" auto_calibrate.R
```

## Critical Files
-   **Simulation**: `KE_Kapiti_eddy.bat`, `KE_Kapiti_eddy.ldndc`
-   **Site/Soil Data**: `KE_Kapiti_site_eddy_Vertis_Gachene.xml`, `KE_Kapiti_siteparameters.xml`
-   **Validation Logic**: `KE_Kapiti_output_eddy/valid8.R`, `KE_Kapiti_output_eddy/run.all.R`
-   **Optimization Logs**: `KE_Kapiti_output_eddy/R_scripts/state.csv`, `KE_Kapiti_output_eddy/metrics.csv`

## Recent Modifications (Session Summary - 17 May 2026)
-   **Convergence Auto-Skip**: Implemented efficiency logic that automatically sets `include <- FALSE` in `Calibr8.csv` for parameters yielding $< 1\%$ improvement or those requiring reversal.
-   **Robustness Fixes**: 
    -   Updated `auto_calibrate.R` to filter out empty rows from `Calibr8.csv`, preventing "invalid description" errors.
    -   Fixed case-sensitivity typos (`LM4` vs `lm4`) and empty sequence errors in `ECT.biomass.in.R`.
-   **Staged Calibration**: The loop now executes in stages, ensuring intra-stage convergence before advancing.
-   **Species-Specific Updates**: Modification of `KE_Kapiti_speciesparameters_GLOBAL.xml` is now isolated to the target `<species>` block.
-   **Command Conventions**: Established `start` and `stop` as formal commands for managing the calibration background process.

### Command Conventions
-   **start**: Execute the `auto_calibrate.R` script in the background.
-   **stop**: Halt the currently running calibration background process.

## Next Steps
-   Use the **start** command to resume calibration sweeps.
-   Monitor `state_new.csv` and `Calibr8.csv` to track auto-disabled parameters.
-   Adjust `delta` or manually re-enable (`include=TRUE`) parameters as needed to refine the optimization trajectory.
