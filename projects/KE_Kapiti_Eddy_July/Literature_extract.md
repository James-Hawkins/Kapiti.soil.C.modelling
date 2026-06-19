# Automated Literature Extraction Process for Vegetation Parameters

This document outlines the automated procedure for collecting vegetation species parameters from scientific and grey literature to populate Landscape-DNDC species data.

## Process Overview

The process iterates through each line of `data/Species_params_GEMINI.csv`, where each row represents a specific physiological parameter for a target plant species.

### 1. Data Structure
The input file defines the target species and parameters using the following columns:
- **Species Identification**: `common_name`, `scientific_name`, `genus`, and `descriptor`.
- **Parameter Identification**: `code` (shorthand), `description` (full name), and `unit`/`unit in words`.
- **Output Columns**: `minimum`, `reference_minimum`, `maximum`, `reference_maximum`, `point_estimate`, and `reference_point_estimate`.

### 2. Tiered Search Strategy
To ensure maximum precision and fallback reliability, the search logic follows a four-tier hierarchy:

1.  **Tier 1 (Specific)**: Search using `common_name` + `scientific_name` + `code` + `description`.
2.  **Tier 2 (Scientific)**: If Tier 1 fails or `common_name` is missing, search using `scientific_name` + `code` + `description`.
3.  **Tier 3 (Genus Level)**: If no results, search using `genus` + `code` + `description`.
4.  **Tier 4 (General Descriptor)**: If still no results, search using `descriptor` + `code` + `description`.

The search is conducted via the **OpenAlex API** (an academic literature index) to retrieve titles and abstracts.

### 3. Data Extraction & LLM Integration
For each parameter, retrieved snippets are processed by an LLM (e.g., Gemini) with a structured prompt. The LLM is tasked with:
- Extracting values for `minimum`, `maximum`, and `point_estimate` consistent with the required units.
- Providing **short-form citations** (e.g., "Author et al., Year") for each specific value.
- Generating **full bibliographic citations** (APA-style) for every source used.

### 4. Citation Management
The process maintains a two-tier citation system:
- **In-situ Citations**: Short-form references are populated directly in the CSV next to their respective values.
- **Full Bibliography**: A comprehensive list of all full citations is displayed at the end of the run and saved to `data/references_citations.txt`.

## Implementation Details

The core logic is implemented in `extract_literature_params.py`.

### How to Run
1. Ensure dependencies are installed: `pip install pandas requests`.
2. Configure the `call_llm` function in the script with your API credentials.
3. Execute the script:
   ```powershell
   python extract_literature_params.py
   ```

### Output Files
- **Populated Data**: `data/Species_params_GEMINI_populated.csv`
- **Bibliography**: `data/references_citations.txt`
- **Simulation XML**: `KE_Kapiti_speciesparameters_GLOBAL_Start.xml` (This file is the permanent record of literature-derived values and is kept separate from the live calibration file `KE_Kapiti_speciesparameters_GLOBAL.xml`).
