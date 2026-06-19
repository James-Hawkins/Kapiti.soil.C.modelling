import pandas as pd
import requests
import os
import json
import time
from typing import List, Dict, Any

# --- Configuration ---
INPUT_FILE = "data/Species_params_GEMINI.csv"
OUTPUT_FILE = "data/Species_params_GEMINI_populated.csv"
CITATIONS_FILE = "data/references_citations.txt"
# This script uses OpenAlex for literature search (free, no key required for low volume)
OPENALEX_API_URL = "https://api.openalex.org/works"

# --- Prompt Template ---
EXTRACT_PROMPT = """
Extract plant physiological parameters from the following literature snippets.
Target Parameter: {param_desc}
Required Units: {unit_words} ({unit})

Context:
{context}

Return a JSON object with the following keys:
- "minimum": The minimum value found (as a string/number).
- "reference_minimum": Short-form citation for the minimum value.
- "maximum": The maximum value found (as a string/number).
- "reference_maximum": Short-form citation for the maximum value.
- "point_estimate": A specific reported value if available.
- "reference_point_estimate": Short-form citation for the point estimate.
- "full_citations": A list of full bibliographic citations for all sources used.

If no data is found for a specific field, return an empty string for that field.
"""

# --- Functions ---

def search_literature(query: str, limit: int = 5) -> List[Dict[str, Any]]:
    """Searches OpenAlex for scientific literature."""
    print(f"Searching: {query}")
    params = {
        'search': query,
        'per_page': limit,
    }
    try:
        response = requests.get(OPENALEX_API_URL, params=params)
        response.raise_for_status()
        data = response.json()
        return data.get('results', [])
    except Exception as e:
        print(f"Error searching OpenAlex: {e}")
        return []

def get_abstract(result: Dict[str, Any]) -> str:
    """Reconstructs abstract from OpenAlex's inverted index."""
    inverted = result.get('abstract_inverted_index')
    if not inverted:
        return ""
    
    # Simple reconstruction
    word_index = {}
    for word, positions in inverted.items():
        for pos in positions:
            word_index[pos] = word
    
    sorted_positions = sorted(word_index.keys())
    return " ".join([word_index[p] for p in sorted_positions])

def call_llm(prompt: str) -> Dict[str, Any]:
    """
    Placeholder for an actual LLM API call (e.g., Gemini or OpenAI).
    Users should implement their own API key and call here.
    """
    # EXAMPLE implementation using a hypothetical library:
    # response = model.generate_content(prompt)
    # return json.loads(response.text)
    
    print("--- LLM PROMPT (Simulated) ---")
    print(prompt[:200] + "...")
    return {
        "minimum": "",
        "reference_minimum": "",
        "maximum": "",
        "reference_maximum": "",
        "point_estimate": "",
        "reference_point_estimate": "",
        "full_citations": []
    }

def main():
    if not os.path.exists(INPUT_FILE):
        print(f"Input file {INPUT_FILE} not found.")
        return

    df = pd.read_csv(INPUT_FILE)
    
    # Ensure all target columns exist
    target_cols = [
        'minimum', 'reference_minimum', 
        'maximum', 'reference_maximum', 
        'point_estimate', 'reference_point_estimate'
    ]
    for col in target_cols:
        if col not in df.columns:
            df[col] = ""

    all_full_citations = set()

    for index, row in df.iterrows():
        common_name = str(row.get('common_name', ''))
        scientific_name = str(row.get('scientific_name', ''))
        genus = str(row.get('genus', ''))
        descriptor = str(row.get('descriptor', ''))
        param_desc = str(row.get('description', ''))
        param_code = str(row.get('code', ''))
        unit_words = str(row.get('unit in words ', ''))
        unit = str(row.get('unit', ''))
        
        print(f"\nProcessing row {index+1}: {common_name} - {param_code} ({param_desc})")
        
        # Tiered Search
        search_results = []
        
        # Tier 1: Common + Scientific + Code
        if common_name != "nan" and scientific_name != "nan":
            query = f'"{common_name}" "{scientific_name}" {param_code} {param_desc}'
            search_results = search_literature(query)
        
        # Tier 2: Scientific + Code (Skip to this if Common Name is missing)
        if not search_results and scientific_name != "nan":
            query = f'"{scientific_name}" {param_code} {param_desc}'
            search_results = search_literature(query)

        # Tier 3: Genus + Code
        if not search_results and genus != "nan":
            query = f'"{genus}" {param_code} {param_desc}'
            search_results = search_literature(query)
            
        # Tier 4: Descriptor + Code
        if not search_results and descriptor != "nan":
            query = f'"{descriptor}" {param_code} {param_desc}'
            search_results = search_literature(query)

        if search_results:
            context = ""
            for res in search_results:
                title = res.get('display_name', '')
                abstract = get_abstract(res)
                context += f"Title: {title}\nAbstract: {abstract}\n\n"
            
            prompt = EXTRACT_PROMPT.format(
                param_desc=param_desc,
                unit_words=unit_words,
                unit=unit,
                context=context
            )
            
            # This requires actual LLM integration
            extracted_data = call_llm(prompt)
            
            df.at[index, 'minimum'] = extracted_data.get('minimum', '')
            df.at[index, 'reference_minimum'] = extracted_data.get('reference_minimum', '')
            df.at[index, 'maximum'] = extracted_data.get('maximum', '')
            df.at[index, 'reference_maximum'] = extracted_data.get('reference_maximum', '')
            df.at[index, 'point_estimate'] = extracted_data.get('point_estimate', '')
            df.at[index, 'reference_point_estimate'] = extracted_data.get('reference_point_estimate', '')
            
            for cit in extracted_data.get('full_citations', []):
                all_full_citations.add(cit)
        
        time.sleep(1) # Courteous rate limiting for OpenAlex

    df.to_csv(OUTPUT_FILE, index=False)
    
    print("\n--- Summary of Citations ---")
    with open(CITATIONS_FILE, 'w', encoding='utf-8') as f:
        for citation in sorted(list(all_full_citations)):
            print(citation)
            f.write(citation + "\n")
            
    print(f"\nPopulated data saved to {OUTPUT_FILE}")
    print(f"Citations saved to {CITATIONS_FILE}")

if __name__ == "__main__":
    main()
