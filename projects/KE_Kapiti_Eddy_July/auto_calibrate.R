library(stringr)
batch_mode <- TRUE

# Paths
calibr8_csv <- 'KE_Kapiti_output_eddy/R_scripts/Calibr8.csv'
metrics_csv <- 'KE_Kapiti_output_eddy/R_scripts/metrics.csv'
state_csv <- 'KE_Kapiti_output_eddy/R_scripts/state_new.csv'
bat_file <- 'KE_Kapiti_eddy.bat'
valid8_r <- 'KE_Kapiti_output_eddy/valid8.R'

get_all_metrics <- function(metric_names) {
  target_metrics <- if (file.exists('metrics.csv')) 'metrics.csv' else metrics_csv
  if (!file.exists(target_metrics)) return(list())
  d <- read.csv(target_metrics)
  
  res <- list()
  for (m in metric_names) {
    if (is.na(m) || m == "" || m == "NA") next
    val <- d[d$osv.variable == m & d$period == 'all', 'nrmse']
    if (length(val) == 0) val <- d[d$osv.variable == m, 'nrmse']
    if (length(val) > 0) res[[m]] <- as.numeric(val[1])
  }
  return(res)
}

get_nrmse <- function(metric_names) {
  vals <- get_all_metrics(metric_names)
  if (length(vals) == 0) return(Inf)
  return(mean(unlist(vals), na.rm = TRUE))
}

get_other_nrmse <- function(target_metric_names) {
  target_metrics <- if (file.exists('metrics.csv')) 'metrics.csv' else metrics_csv
  if (!file.exists(target_metrics)) return(NA)
  d <- read.csv(target_metrics)

  d_all <- d[d$period == 'all', ]
  if (nrow(d_all) == 0) d_all <- d

  other_vars <- setdiff(unique(d_all$osv.variable), target_metric_names)
  other_vars <- other_vars[!is.na(other_vars) & other_vars != ""]

  if (length(other_vars) == 0) return(NA)

  vals <- d_all[d_all$osv.variable %in% other_vars, 'nrmse']
  if (length(vals) == 0) return(NA)
  return(mean(as.numeric(vals), na.rm = TRUE))
}

get_pheno_errors <- function(metric_names) {
  target_metrics <- if (file.exists('metrics.csv')) 'metrics.csv' else metrics_csv
  if (!file.exists(target_metrics)) return(list())
  d <- read.csv(target_metrics)
  
  res <- list()
  for (m in metric_names) {
    if (is.na(m) || m == "" || m == "NA") {
        res[[length(res) + 1]] <- NA
        next
    }
    if (!("error.phenological" %in% names(d))) {
        res[[length(res) + 1]] <- FALSE
        next
    }
    val <- d[d$osv.variable == m & d$period == 'all', 'error.phenological']
    if (length(val) == 0) val <- d[d$osv.variable == m, 'error.phenological']
    if (length(val) > 0) res[[length(res) + 1]] <- as.logical(val[1]) else res[[length(res) + 1]] <- FALSE
  }
  return(res)
}

update_xml <- function(xml_file, parameter, delta, species_name = NA, direction = "higher", param_min = NA, param_max = NA, param_class = NA, soil_layer = NA) {
  if (!file.exists(xml_file)) {
    options <- c(paste0(xml_file, ".xml"), paste0("KE_Kapiti_", xml_file), paste0("KE_Kapiti_", xml_file, ".xml"))
    for (opt in options) if (file.exists(opt)) { xml_file <- opt; break }
  }
  
  lines <- readLines(xml_file, warn = FALSE)
  content <- paste(lines, collapse = "\n")
  delta_mult <- if (direction == "lower") (1 - delta) else (1 + delta)
  hit_bound <- FALSE
  
  is_global_species <- str_detect(xml_file, "speciesparameters_GLOBAL.xml")
  is_event_file <- str_detect(xml_file, "events_eddy.xml")
  
  if (!is.na(param_class) && param_class == "veg.mix") {
    # Special handling for vegetation mix (coordinated Excel + multiple XML update)
    cmd <- paste("python update_veg_mix.py update", shQuote(species_name), shQuote(direction), delta, 
                 ifelse(is.na(param_min), "NA", param_min), ifelse(is.na(param_max), "NA", param_max))
    res <- system(cmd, intern = TRUE)
    res_line <- res[grep("^RESULT:", res)]
    if (length(res_line) > 0) {
      parts <- strsplit(sub("^RESULT: ", "", res_line), ",")[[1]]
      old_val <- as.numeric(trimws(parts[1]))
      new_val <- as.numeric(trimws(parts[2]))
      hit_bound <- as.logical(trimws(parts[3]))
      if (new_val == old_val) return(list(old = old_val, new = new_val, file = xml_file, changed = FALSE, hit_bound = hit_bound))
      return(list(old = old_val, new = new_val, file = xml_file, changed = TRUE, hit_bound = hit_bound))
    } else {
      stop(paste("veg.mix update failed:", paste(res, collapse="\n")))
    }
  } else if (is_global_species && !is.na(species_name) && species_name != "" && species_name != "NA") {
    start_idx <- which(str_detect(lines, paste0('<species[^>]*name="', species_name, '"')))
    if (length(start_idx) == 0) stop(paste("Species", species_name, "not found"))
    
    end_indices <- which(str_detect(lines, "</species>"))
    end_idx <- end_indices[end_indices > start_idx[1]][1]
    if (is.na(end_idx)) stop("Closing tag </species> not found")
    
    target_lines <- lines[start_idx[1]:end_idx]
    target_content <- paste(target_lines, collapse = "\n")
    pattern_par <- paste0('(<par name="', parameter, '" value=")([^"]+)(")')
    
    matches <- str_match(target_content, pattern_par)
    if (is.na(matches[1, 1])) stop(paste("Parameter", parameter, "not found for species", species_name))
    
    old_val <- as.numeric(matches[1, 3])
    new_val <- old_val * delta_mult
    
    if (!is.na(param_min) && new_val <= param_min) { new_val <- param_min; hit_bound <- TRUE }
    if (!is.na(param_max) && new_val >= param_max) { new_val <- param_max; hit_bound <- TRUE }
    
    if (new_val == old_val) return(list(old = old_val, new = new_val, file = xml_file, changed = FALSE, hit_bound = hit_bound))
    
    target_content <- str_replace(target_content, pattern_par, paste0("\\1", round(new_val, 5), "\\3"))
    file.copy(xml_file, paste0(xml_file, ".bak"), overwrite = TRUE)
    lines[start_idx[1]:end_idx] <- str_split(target_content, "\n")[[1]]
    writeLines(lines, xml_file)
  } else if (is_event_file && !is.na(species_name) && species_name != "" && species_name != "NA") {
    start_idx <- which(str_detect(lines, paste0('<plant[^>]*type="', species_name, '"')))
    if (length(start_idx) == 0) stop(paste("Species", species_name, "not found in events file"))
    
    end_indices <- which(str_detect(lines, "</plant>"))
    end_idx <- end_indices[end_indices > start_idx[1]][1]
    if (is.na(end_idx)) stop("Closing tag </plant> not found")
    
    target_lines <- lines[start_idx[1]:end_idx]
    target_content <- paste(target_lines, collapse = "\n")
    pattern_par <- paste0('(', parameter, '=")([^"]+)(")')
    
    matches <- str_match(target_content, pattern_par)
    if (is.na(matches[1, 1])) stop(paste("Parameter", parameter, "not found for species", species_name))
    
    old_val <- as.numeric(matches[1, 3])
    new_val <- old_val * delta_mult
    
    if (!is.na(param_min) && new_val <= param_min) { new_val <- param_min; hit_bound <- TRUE }
    if (!is.na(param_max) && new_val >= param_max) { new_val <- param_max; hit_bound <- TRUE }
    
    if (new_val == old_val) return(list(old = old_val, new = new_val, file = xml_file, changed = FALSE, hit_bound = hit_bound))
    
    target_content <- str_replace(target_content, pattern_par, paste0("\\1", round(new_val, 5), "\\3"))
    file.copy(xml_file, paste0(xml_file, ".bak"), overwrite = TRUE)
    lines[start_idx[1]:end_idx] <- str_split(target_content, "\n")[[1]]
    writeLines(lines, xml_file)
  } else if (!is.na(param_class) && param_class == "soil" && !is.na(soil_layer) && soil_layer != "" && soil_layer != "NA") {
    depth_mm <- as.numeric(str_extract(soil_layer, "[0-9.]+")) * 10
    pattern_layer <- paste0('<layer[^>]*depth="', depth_mm, '"')
    line_idx <- which(str_detect(lines, pattern_layer))
    if (length(line_idx) == 0) stop(paste("Soil layer with depth", depth_mm, "not found in", xml_file))
    
    target_line <- lines[line_idx[1]]
    pattern_par <- paste0('(', parameter, '=")([^"]+)(")')
    matches <- str_match(target_line, pattern_par)
    if (is.na(matches[1, 1])) stop(paste("Parameter", parameter, "not found in layer depth", depth_mm))
    
    old_val <- as.numeric(matches[1, 3])
    new_val <- old_val * delta_mult
    if (!is.na(param_min) && new_val <= param_min) { new_val <- param_min; hit_bound <- TRUE }
    if (!is.na(param_max) && new_val >= param_max) { new_val <- param_max; hit_bound <- TRUE }
    if (new_val == old_val) return(list(old = old_val, new = new_val, file = xml_file, changed = FALSE, hit_bound = hit_bound))
    
    lines[line_idx[1]] <- str_replace(target_line, pattern_par, paste0("\\1", round(new_val, 5), "\\3"))
    file.copy(xml_file, paste0(xml_file, ".bak"), overwrite = TRUE)
    writeLines(lines, xml_file)
  } else {
    pattern_site <- paste0('(<par name="', parameter, '" value=")([^"]+)(")')
    pattern_soil <- paste0('(', parameter, '=")([^"]+)(")')
    
    if (str_detect(content, pattern_site)) {
      matches <- str_match(content, pattern_site)
      old_val <- as.numeric(matches[1, 3]); new_val <- old_val * delta_mult
      if (!is.na(param_min) && new_val <= param_min) { new_val <- param_min; hit_bound <- TRUE }
      if (!is.na(param_max) && new_val >= param_max) { new_val <- param_max; hit_bound <- TRUE }
      if (new_val == old_val) return(list(old = old_val, new = new_val, file = xml_file, changed = FALSE, hit_bound = hit_bound))
      content <- str_replace(content, pattern_site, paste0("\\1", round(new_val, 5), "\\3"))
    } else if (str_detect(content, pattern_soil)) {
      matches <- str_match(content, pattern_soil)
      old_val <- as.numeric(matches[1, 3]); new_val <- old_val * delta_mult
      if (!is.na(param_min) && new_val <= param_min) { new_val <- param_min; hit_bound <- TRUE }
      if (!is.na(param_max) && new_val >= param_max) { new_val <- param_max; hit_bound <- TRUE }
      if (new_val == old_val) return(list(old = old_val, new = new_val, file = xml_file, changed = FALSE, hit_bound = hit_bound))
      content <- str_replace(content, pattern_soil, paste0("\\1", round(new_val, 5), "\\3"))
    } else {
      stop(paste("Parameter", parameter, "not found"))
    }
    file.copy(xml_file, paste0(xml_file, ".bak"), overwrite = TRUE)
    writeLines(content, xml_file)
  }
  print(paste("Updated", parameter, "to", new_val, "(", direction, ")"))
  return(list(old = old_val, new = new_val, file = xml_file, changed = TRUE, hit_bound = hit_bound))
}

read_xml_value <- function(xml_file, parameter, species_name = NA, param_class = NA, soil_layer = NA) {
  if (!file.exists(xml_file)) {
    options <- c(paste0(xml_file, ".xml"), paste0("KE_Kapiti_", xml_file), paste0("KE_Kapiti_", xml_file, ".xml"))
    for (opt in options) if (file.exists(opt)) { xml_file <- opt; break }
  }
  lines <- readLines(xml_file, warn = FALSE)
  content <- paste(lines, collapse = "\n")
  is_global_species <- str_detect(xml_file, "speciesparameters_GLOBAL.xml")
  is_event_file <- str_detect(xml_file, "events_eddy.xml")
  
  if (!is.na(param_class) && param_class == "veg.mix") {
    cmd <- paste("python update_veg_mix.py read", shQuote(species_name))
    res <- system(cmd, intern = TRUE)
    res_line <- res[grep("^RESULT:", res)]
    if (length(res_line) > 0) return(as.numeric(sub("^RESULT: ", "", res_line)))
    return(NA)
  } else if (is_global_species && !is.na(species_name) && species_name != "" && species_name != "NA") {
    start_idx <- which(str_detect(lines, paste0('<species[^>]*name="', species_name, '"')))
    if (length(start_idx) == 0) return(NA)
    
    end_indices <- which(str_detect(lines, "</species>"))
    end_idx <- end_indices[end_indices > start_idx[1]][1]
    if (is.na(end_idx)) return(NA)
    
    target_content <- paste(lines[start_idx[1]:end_idx], collapse = "\n")
    pattern_par <- paste0('<par name="', parameter, '" value="([^"]+)"')
    m <- str_match(target_content, pattern_par)
    return(if(!is.na(m[1,1])) as.numeric(m[1, 2]) else NA)
  } else if (is_event_file && !is.na(species_name) && species_name != "" && species_name != "NA") {
    start_idx <- which(str_detect(lines, paste0('<plant[^>]*type="', species_name, '"')))
    if (length(start_idx) == 0) return(NA)
    
    end_indices <- which(str_detect(lines, "</plant>"))
    end_idx <- end_indices[end_indices > start_idx[1]][1]
    if (is.na(end_idx)) return(NA)
    
    target_content <- paste(lines[start_idx[1]:end_idx], collapse = "\n")
    pattern_par <- paste0(parameter, '="([^"]+)"')
    m <- str_match(target_content, pattern_par)
    return(if(!is.na(m[1,1])) as.numeric(m[1, 2]) else NA)
  } else if (!is.na(param_class) && param_class == "soil" && !is.na(soil_layer) && soil_layer != "" && soil_layer != "NA") {
    depth_mm <- as.numeric(str_extract(soil_layer, "[0-9.]+")) * 10
    pattern_layer <- paste0('<layer[^>]*depth="', depth_mm, '"')
    line_idx <- which(str_detect(lines, pattern_layer))
    if (length(line_idx) == 0) return(NA)
    pattern_par <- paste0(parameter, '="([^"]+)"')
    m <- str_match(lines[line_idx[1]], pattern_par)
    return(if(!is.na(m[1,1])) as.numeric(m[1, 2]) else NA)
  } else {
    pattern_site <- paste0('<par name="', parameter, '" value="([^"]+)"')
    pattern_soil <- paste0(parameter, '="([^"]+)"')
    if (str_detect(content, pattern_site)) return(as.numeric(str_match(content, pattern_site)[1, 2]))
    if (str_detect(content, pattern_soil)) return(as.numeric(str_match(content, pattern_soil)[1, 2]))
    return(NA)
  }
}

# Load data and setup baselines
calibr8_data <- read.csv(calibr8_csv, check.names = FALSE)
calibr8_data <- calibr8_data[!is.na(calibr8_data$parameter) & calibr8_data$parameter != "", ]

unique_files <- unique(calibr8_data$file); resolved_files <- setNames(unique_files, unique_files)
for (f in unique_files) {
    if (is.na(f) || f == "") next
    target <- f
    if (!file.exists(target)) {
        for (opt in c(paste0(f, ".xml"), paste0("KE_Kapiti_", f), paste0("KE_Kapiti_", f, ".xml"))) if (file.exists(opt)) { target <- opt; break }
    }
    resolved_files[f] <- target
    
    # Initialization and Backup Logic (Requirement: 24 May 2026)
    backup_file <- str_replace(target, "\\.xml$", "_backup.xml")
    baseline_file <- str_replace(target, "\\.xml$", "_baseline.xml")
    
    # Create backup from current state
    if (file.exists(target)) {
        file.copy(target, backup_file, overwrite = TRUE)
        # Update baseline to current state so iterations start from this initialization point
        file.copy(target, baseline_file, overwrite = TRUE)
        print(paste("Initialized calibration from", target, "and created backup:", backup_file))
    }
}

# Identification of metric columns
metric_cols <- names(calibr8_data)[str_detect(names(calibr8_data), "^(validation|calibration)\\.metric\\.")]

# Helper to get the unique key for a parameter (handling GLOBAL species and soil layers)
get_param_key <- function(p, s, f, class = NA, soil_layer = NA) {
  if (str_detect(f, "speciesparameters_GLOBAL") && !is.na(s) && s != "" && s != "NA") {
    return(paste0(p, "_", s))
  }
  if (!is.na(class) && class == "soil" && !is.na(soil_layer) && soil_layer != "" && soil_layer != "NA") {
    clean_layer <- str_replace_all(soil_layer, " ", "_")
    return(paste0(p, "_", clean_layer))
  }
  return(p)
}

# Generate definitive column list and initial state from baselines
current_parameter_state <- list()
all_param_keys <- character()
for (i in 1:nrow(calibr8_data)) {
    pk <- get_param_key(calibr8_data$parameter[i], calibr8_data$species[i], calibr8_data$file[i], calibr8_data$class[i], calibr8_data$soil.layer[i])
    all_param_keys <- c(all_param_keys, pk)
    current_parameter_state[[pk]] <- read_xml_value(resolved_files[calibr8_data$file[i]], calibr8_data$parameter[i], calibr8_data$species[i], calibr8_data$class[i], calibr8_data$soil.layer[i])
}
all_param_keys <- unique(all_param_keys)
target_cols <- c("iteration", all_param_keys)

# Load/clean and SYNC state_new.csv
if (file.exists(state_csv)) {
  state <- read.csv(state_csv, check.names = FALSE)
  if (nrow(state) > 0 && (names(state)[1] == "X" || names(state)[1] == "")) state <- state[, -1, drop = FALSE]
  
  # Filter out empty rows
  param_cols_in_state <- setdiff(names(state), "iteration")
  if (length(param_cols_in_state) > 0 && nrow(state) > 0) {
      is_empty <- apply(state[, param_cols_in_state, drop=FALSE], 1, function(x) all(is.na(x) | x == ""))
      state <- state[!is_empty, , drop=FALSE]
  }
  
  # ADD missing columns
  for (col in target_cols) {
    if (!(col %in% names(state))) {
      state[[col]] <- if (nrow(state) > 0) NA else logical(0)
    }
  }
  # PRUNE obsolete columns and FORCE order
  state <- state[, target_cols, drop = FALSE]
} else {
  state <- data.frame(matrix(ncol = length(target_cols), nrow = 0))
  colnames(state) <- target_cols
}

stages <- sort(unique(calibr8_data$stage))
iteration_counter <- if(nrow(state) > 0 && "iteration" %in% names(state)) max(state$iteration, na.rm = TRUE) + 1 else 1

# Check for existing pheno errors to initialize mode
pheno_error_active <- FALSE
existing_errors <- get_pheno_errors(unique(unlist(calibr8_data[, metric_cols])))
if (any(unlist(existing_errors) == TRUE, na.rm = TRUE)) {
    pheno_error_active <- TRUE
    print(">>> PHENOLOGICAL ERROR DETECTED ON STARTUP. Priority shifted to pheno parameters. <<<")
}

for (current_stage in stages) {
  print(paste("##### STARTING STAGE", current_stage, "#####"))
  stage_converged <- FALSE; sweep_count <- 1
  while (!stage_converged) {
    print(paste("=== Stage", current_stage, "- Sweep", sweep_count, "==="))
    max_improvement_in_sweep <- 0
    stage_indices <- which(calibr8_data$stage == current_stage)
    
    for (idx in stage_indices) {
      param_info <- calibr8_data[idx, ]
      if (!is.na(param_info$include) && (as.character(param_info$include) == "FALSE" || param_info$include == FALSE)) next
      
      # JUMP Logic: Branching based on phenological errors
      is_pheno_param <- if("parameter.phenological" %in% names(param_info)) as.logical(param_info$parameter.phenological) else FALSE
      if (is.na(is_pheno_param)) is_pheno_param <- FALSE
      
      if (pheno_error_active && !is_pheno_param) {
          print(paste("Jumping", param_info$parameter, ": Phenological error active, prioritizing pheno params.")); next
      }
      if (!pheno_error_active && is_pheno_param) {
          # Only skip if there are non-pheno parameters still to be processed in this stage
          non_pheno_left <- any(calibr8_data$include[stage_indices] == TRUE & !as.logical(calibr8_data$parameter.phenological[stage_indices]))
          if (non_pheno_left) {
              print(paste("Skipping", param_info$parameter, ": No phenological error active, focusing on non-pheno params.")); next
          }
      }

      if (!is.na(param_info$status) && param_info$status == "BOUND_REACHED") {
        print(paste("Skipping", param_info$parameter, ": Bound already reached.")); next
      }
      
      parameter <- param_info$parameter; xml_file <- param_info$file; delta <- param_info$delta
      param_class <- if("class" %in% names(param_info)) param_info$class else NA
      soil_layer <- if("soil.layer" %in% names(param_info)) param_info$soil.layer else NA
      param_min <- if("param.min" %in% names(param_info)) as.numeric(param_info$"param.min") else NA
      param_max <- if("param.max" %in% names(param_info)) as.numeric(param_info$"param.max") else NA
      species <- if("species" %in% names(param_info)) param_info$species else NA
      direction <- if("direction" %in% names(param_info)) param_info$direction else "higher"
      param_key <- get_param_key(parameter, species, xml_file, param_class, soil_layer)
      current_metrics <- as.character(param_info[metric_cols]); current_metrics <- current_metrics[!is.na(current_metrics) & current_metrics != ""]
      
      attempt <- 1; keep_going <- TRUE
      baseline_nrmse <- get_nrmse(current_metrics); baseline_others_nrmse <- get_other_nrmse(current_metrics)
      
      while (attempt <= 2 && keep_going) {
        print(paste("Iteration", iteration_counter, "(", attempt, "):", param_key, "Dir:", direction))
        update_res <- try(update_xml(xml_file, parameter, delta, species, direction, param_min, param_max, param_class, soil_layer), silent = TRUE)
        if (inherits(update_res, "try-error")) { 
          print(paste("Skipping", param_key, ":", attr(update_res, "condition")$message))
          keep_going <- FALSE; next 
        }
        
        if (!update_res$changed) {
          if (attempt == 1) {
            print("Target already at bound. Retrying opposite direction..."); attempt <- 2
            direction <- if(direction == "higher") "lower" else "higher"; next
          } else {
            status <- "BOUND_REACHED"; final_val <- update_res$old; keep_going <- FALSE; next
          }
        }
        
        print("Running simulation/validation..."); shell(paste("echo. |", bat_file))
        old_wd <- getwd(); setwd('KE_Kapiti_output_eddy')
        if (exists("d.all")) rm("d.all", envir = .GlobalEnv)
        assign("batch_mode", TRUE, envir = .GlobalEnv); try(source('valid8.R', local = TRUE)); setwd(old_wd)
        
        metric_results <- get_all_metrics(current_metrics)
        pheno_errors <- get_pheno_errors(current_metrics)
        
        # Update pheno error flags in calibr8_data
        pheno_cols <- c("metric.1.error.phenological", "metric.2.error.phenological", "metric.3.error.phenological")
        for (i in 1:length(pheno_errors)) {
            if (i <= length(pheno_cols)) {
                calibr8_data[idx, pheno_cols[i]] <- as.character(pheno_errors[[i]])
            }
        }
        
        # Determine if we STAY in pheno mode or SWITCH
        if (any(unlist(pheno_errors) == TRUE, na.rm = TRUE)) {
            pheno_error_active <- TRUE
            print(">>> PHENOLOGICAL ERROR DETECTED. Priority shifted to pheno parameters. <<<")
        } else {
            pheno_error_active <- FALSE
        }

        new_nrmse <- if(length(metric_results) > 0) mean(unlist(metric_results), na.rm = TRUE) else Inf
        new_others_nrmse <- get_other_nrmse(current_metrics)
        improvement_pct <- if(is.finite(baseline_nrmse) && baseline_nrmse > 0) (baseline_nrmse - new_nrmse) / baseline_nrmse * 100 else 0
        status_others <- if(is.na(new_others_nrmse) || is.na(baseline_others_nrmse)) "NA" else if (new_others_nrmse > baseline_others_nrmse) "INCREASED" else "NOT INCREASED"
        
        if (improvement_pct > 0 && status_others != "INCREASED") {
          status <- if (update_res$hit_bound) "BOUND_REACHED" else if (improvement_pct >= 1.0) "KEPT (>= 1% improvement)" else "KEPT (< 1% improvement)"
          final_val <- update_res$new; max_improvement_in_sweep <- max(max_improvement_in_sweep, improvement_pct); keep_going <- FALSE
        } else if (attempt == 1 && improvement_pct <= 0) {
          print("Target degraded. Retrying opposite direction..."); file.copy(paste0(update_res$file, ".bak"), update_res$file, overwrite = TRUE)
          direction <- if(direction == "higher") "lower" else "higher"; attempt <- 2
        } else {
          status <- "REVERTED"; final_val <- update_res$old; file.copy(paste0(update_res$file, ".bak"), update_res$file, overwrite = TRUE); keep_going <- FALSE
        }
      }
      
      if (exists("update_res") && !inherits(update_res, "try-error")) {
          calibr8_data[idx, c("direction", "old_value", "new_value", "nrmse_baseline", "nrmse_new", "nrmse_others_baseline", "nrmse_others_new", "improvement_pct", "status", "status_others")] <- 
              list(direction, round(update_res$old, 5), round(update_res$new, 5), round(baseline_nrmse, 5), round(new_nrmse, 5), round(baseline_others_nrmse, 5), round(new_others_nrmse, 5), round(improvement_pct, 2), status, status_others)
          
          if (improvement_pct < 1.0 || status == "REVERTED" || status == "BOUND_REACHED") {
              calibr8_data$include[idx] <- FALSE
              print(paste(">>> Convergence detected for", param_key, "(Improvement:", round(improvement_pct, 2), "%, Status:", status, "). Setting include to FALSE. <<<"))
          }
          
          try(write.csv(calibr8_data, calibr8_csv, row.names = FALSE, na=""))
          
          current_parameter_state[[param_key]] <- round(final_val, 5)
          new_row <- data.frame(iteration = iteration_counter, stringsAsFactors = FALSE)
          for (pk in all_param_keys) new_row[[pk]] <- current_parameter_state[[pk]]
          target_cols <- c("iteration", all_param_keys)
          for (col in target_cols) if (!(col %in% names(state))) state[[col]] <- NA
          state <- state[, target_cols, drop = FALSE]; new_row <- new_row[, target_cols, drop = FALSE]
          state <- rbind(state, new_row); try(write.csv(state, state_csv, row.names = FALSE, na=""))
          iteration_counter <- iteration_counter + 1
      }
      if (exists("update_res")) rm(update_res)
    }
    if (max_improvement_in_sweep < 1.0) stage_converged <- TRUE else sweep_count <- sweep_count + 1
  }
}
print("Calibration procedure finished.")
