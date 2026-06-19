# Objective
Automate the parameterization of hybrid species (`hybrid.1` to `hybrid.4`) in `KE_Kapiti_speciesparameters_GLOBAL.xml` based on data from `data/Kapiti_LDNDC_data_Master.xlsx` (sheet `Params_specs_ALL`).

# Requirements
- Source Data: `data/Kapiti_LDNDC_data_Master.xlsx`, sheet `Params_specs_ALL`.
- Parameter Names: Column 1.
- Hybrid Values: Columns 24 (`hybrid.1`), 25 (`hybrid.2`), 26 (`hybrid.3`), 27 (`hybrid.4`).
- Inclusion Logic: Column 28 (`hybrid.include`). If FALSE, skip the row.
- Target: `KE_Kapiti_speciesparameters_GLOBAL.xml`.
- Creation Logic:
  - If a species block (e.g., `<species name="hybrid.1">`) is missing, create it.
  - If a parameter `<par name="...">` is missing within a species block, create it.
- Environment: R script using `readxl` and `xml2`.

# Implementation Plan
1. **Initialize R Script (`update_hybrid_params.R`):**
   - Load libraries `readxl`, `xml2`, and `dplyr`.
2. **Read Excel Data:**
   - Use `read_excel` to read columns 1, 24-28 from the target sheet.
   - Filter rows where `hybrid.include` is not FALSE.
3. **Parse XML:**
   - Use `read_xml` to load the current global species parameter file.
4. **Iterative Update:**
   - For each hybrid species (hybrid.1, hybrid.2, hybrid.3, hybrid.4):
     - Identify or create the species node.
     - For each row in the Excel data:
       - Update or add the `<par>` element with the corresponding value.
5. **Save Changes:**
   - Write the updated XML back to `KE_Kapiti_speciesparameters_GLOBAL.xml`.
   - Create a backup before saving.

# Verification
- Run the R script.
- Verify `KE_Kapiti_speciesparameters_GLOBAL.xml` for the presence and correct values of hybrid parameters.
