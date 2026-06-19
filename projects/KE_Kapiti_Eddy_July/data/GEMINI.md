# Project Guidelines (Data Folder Supplement)

## Data Loading Workflow (Excel to XML)
To protect native Excel averaging and automated computations, always follow this workflow:
1. **Load Data to Master:** Populate updated parameters from `Species_params_GEMINI.xlsm` into `Weighted_species_params_master.xlsm` (Macro-Enabled Workbook) using `python sync_xlsm_to_master.py`.
   - **MAX/MIN Logic:** For parameters with 'MAX' or 'MIN' in their name (e.g., `GSMAX`, `SLAMAX`, `WUECMIN`), the script uses the `maximum` or `minimum` column from the source respectively, instead of the `midpoint`.
2. **Create Child & Update XML:** Run `python sync_excel_to_xml.py`. This script performs the following:
   - **Hybrid Calculation:** Recalculates weighted averages for all Hybrid functional groups based on `fractionalcover`.
   - **Biomass Fraction Rule:** Ensures the sum of `FRACTION_FOLIAGE`, `FRACTION_ROOT`, and `FRACTION_FRUIT` is < 1.0. If the sum >= 1.0, `FRACTION_FRUIT` is automatically reduced to satisfy the constraint.
   - **'include.xml' Filter:** Only parameters marked as `TRUE` (or `1`) in the `include.xml` column of the master file are copied to the XML. Parameters marked `FALSE` (or `0`) are strictly skipped.
   - **Child Synchronization:** Generates/Refreshes `Weighted_species_params_child.xlsx` with all calculated and adjusted values.
   - **XML Synchronization:** Updates `KE_Kapiti_speciesparameters_GLOBAL.xml` drawing values strictly from the adjusted dataset.
