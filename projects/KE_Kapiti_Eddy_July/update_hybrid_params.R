library(readxl)
library(xml2)
library(dplyr)

# Paths
excel_path <- "data/Kapiti_LDNDC_data_Master.xlsx"
xml_path <- "KE_Kapiti_speciesparameters_GLOBAL.xml"
sheet_name <- "Params_specs_ALL"

# 1. Read Excel Data
cat("Reading Excel data from sheet:", sheet_name, "\n")
df <- read_excel(excel_path, sheet = sheet_name, col_names = TRUE)

find_best_column <- function(data, name_pattern) {
  cols <- which(grepl(name_pattern, names(data)))
  if (length(cols) == 0) return(NULL)
  scores <- sapply(cols, function(idx) {
    vals <- data[[idx]][1:min(20, nrow(data))]
    sum(!is.na(as.numeric(as.character(vals))), na.rm = TRUE)
  })
  return(cols[which.max(scores)])
}

param_col_idx <- 1
h1_idx <- find_best_column(df, "^hybrid\\.1")
h2_idx <- find_best_column(df, "^hybrid\\.2")
h3_idx <- find_best_column(df, "^hybrid\\.3")
h4_idx <- find_best_column(df, "^hybrid\\.4")
inc_idx <- find_best_column(df, "^hybrid\\.include")

if (is.null(h1_idx) || is.null(inc_idx)) {
    cat("Warning: Could not find columns by name clearly. Using offsets.\n")
    h1_idx <- 30; h2_idx <- 31; h3_idx <- 32; h4_idx <- 33; inc_idx <- 34
}

# Filter data
clean_df <- data.frame(
  param = as.character(df[[param_col_idx]]),
  hybrid.1 = df[[h1_idx]],
  hybrid.2 = df[[h2_idx]],
  hybrid.3 = df[[h3_idx]],
  hybrid.4 = df[[h4_idx]],
  include = df[[inc_idx]],
  stringsAsFactors = FALSE
)

clean_df <- clean_df %>%
  filter(!is.na(param) & param != "" & param != "NA") %>%
  filter(!is.na(include) & as.character(include) != "FALSE")

cat("Rows to process:", nrow(clean_df), "\n")

# 2. Parse XML
cat("Reading XML file:", xml_path, "\n")
xml <- read_xml(xml_path)

# 3. Update/Create logic
hybrids <- c("hybrid.1", "hybrid.2", "hybrid.3", "hybrid.4")

for (h_name in hybrids) {
  cat("Processing species:", h_name, "\n")
  
  # Find species node
  species_node <- xml_find_first(xml, paste0("//species[@name='", h_name, "']"))
  
  if (is.na(xml_path(species_node))) {
    cat("  Species node missing. Creating new node for", h_name, "\n")
    # Search for the container node
    container <- xml_find_first(xml, "//speciesparameters")
    if (is.na(xml_path(container))) {
        stop("Could not find <speciesparameters> container node in XML.")
    }
    
    species_node <- xml_add_child(container, "species", 
                                  group = "grass", 
                                  mnemonic = toupper(gsub("\\.", "", h_name)), 
                                  name = h_name, 
                                  parent = "PERG")
  }
  
  # Update parameters for this species
  updated_count <- 0
  created_count <- 0
  
  for (i in 1:nrow(clean_df)) {
    p_name <- clean_df$param[i]
    p_val <- clean_df[[h_name]][i]
    
    if (length(p_val) == 0 || is.na(p_val)) next
    
    p_val_num <- as.numeric(as.character(p_val))
    if (is.na(p_val_num)) next
    
    p_val_str <- as.character(round(p_val_num, 5))
    
    par_node <- xml_find_first(species_node, paste0(".//par[@name='", p_name, "']"))
    
    if (is.na(xml_path(par_node))) {
      xml_add_child(species_node, "par", name = p_name, value = p_val_str)
      created_count <- created_count + 1
    } else {
      xml_set_attr(par_node, "value", p_val_str)
      updated_count <- updated_count + 1
    }
  }
  cat("  Summary for", h_name, ": Updated", updated_count, ", Created", created_count, "\n")
}

# 4. Save
backup_path <- paste0(xml_path, ".bak_hybrid")
file.copy(xml_path, backup_path, overwrite = TRUE)
cat("Backup created at:", backup_path, "\n")

write_xml(xml, xml_path)
cat("Updated XML saved to:", xml_path, "\n")
