# cd /data/gpfs/projects/punim2885/Xuan/ENV
# conda create -p llm_dts_conda
# conda activate /data/gpfs/projects/punim2885/Xuan/ENV/llm_dts_conda

# deactivate
# source /data/gpfs/projects/punim2885/Xuan/env/llm_dts/bin/activate


#=========================Loading Packages===================================
suppressMessages(library("optparse"))
suppressMessages(library("tidyverse"))
suppressMessages(library("striprtf"))
suppressMessages(library("stringr"))
#=========================Function Definition================================
# Function to parse RTF dictionary into structured data frame
# Returns a data frame with columns: position, variable, variable_label, 
# variable_type, spss_level, and value_labels (nested list of data frames)
parse_dictionary <- function(dictionary_text) {
  # Combine all lines into a single string for easier parsing
  dict_text <- paste(dictionary_text, collapse = "\n")
  
  # Find all variable sections (they start with "Pos. =")
  # Pattern: Pos. = <number> Variable = <varname> Variable label = <label>
  pos_pattern <- "Pos\\.\\s*=\\s*(\\d+)\\s+Variable\\s*=\\s*(\\S+)\\s+Variable label\\s*=\\s*([^\\n\\r]+)"
  
  # Extract all variable information
  matches <- stringr::str_match_all(dict_text, pos_pattern)[[1]]
  
  if (nrow(matches) == 0) {
    warning("No variables found in dictionary. Check if the format matches expected pattern.")
    return(data.frame(
      position = integer(0),
      variable = character(0),
      variable_label = character(0),
      variable_type = character(0),
      spss_level = character(0),
      #value_labels = I(list())
      value_labels =  character(0)
    ))
  }
  
  # Create base data frame with variable info
  var_df <- data.frame(
    position = as.integer(matches[, 2]),
    variable = matches[, 3],
    variable_label = trimws(matches[, 4]),
    stringsAsFactors = FALSE
  )
  
  # Extract variable type information for each variable
  # Pattern: "This variable is <type>, the SPSS measurement level is <level>"
  var_df$variable_type <- NA_character_
  var_df$spss_level <- NA_character_
  
  # Split text by variable sections and extract type info for each
  for (i in 1:nrow(var_df)) {
    # Find the section for this variable
    var_start <- stringr::str_locate(dict_text, 
      paste0("Pos\\.\\s*=\\s*", var_df$position[i], "\\s+Variable\\s*=\\s*", var_df$variable[i]))[1]
    
    if (!is.na(var_start)) {
      # Find the next variable or end of text
      next_var_start <- if (i < nrow(var_df)) {
        stringr::str_locate(dict_text, 
          paste0("Pos\\.\\s*=\\s*", var_df$position[i+1], "\\s+Variable\\s*=\\s*", var_df$variable[i+1]))[1]
      } else {
        nchar(dict_text) + 1
      }
      
      if (is.na(next_var_start)) next_var_start <- nchar(dict_text) + 1
      
      var_section <- substr(dict_text, var_start, next_var_start - 1)
      
      # Extract type information
      type_match <- stringr::str_match(var_section, 
        "This variable is\\s+([^,]+),\\s+the SPSS measurement level is\\s+([^\\n\\r]+)")
      if (!is.na(type_match[1, 1])) {
        var_df$variable_type[i] <- trimws(type_match[1, 2])
        var_df$spss_level[i] <- trimws(stringr::str_replace(type_match[1, 3], "\\s+$", ""))
      }
    }
  }
  
  # Extract value labels for each variable
  value_labels_list <- vector("list", nrow(var_df))
  names(value_labels_list) <- var_df$variable
  
  # Extract value labels within each variable's section
  for (i in 1:nrow(var_df)) {
    # Find the section for this variable (already computed above)
    var_start <- stringr::str_locate(dict_text, 
      paste0("Pos\\.\\s*=\\s*", var_df$position[i], "\\s+Variable\\s*=\\s*", var_df$variable[i]))[1]
    
    if (!is.na(var_start)) {
      # Find the next variable or end of text
      next_var_start <- if (i < nrow(var_df)) {
        stringr::str_locate(dict_text, 
          paste0("Pos\\.\\s*=\\s*", var_df$position[i+1], "\\s+Variable\\s*=\\s*", var_df$variable[i+1]))[1]
      } else {
        nchar(dict_text) + 1
      }
      
      if (is.na(next_var_start)) next_var_start <- nchar(dict_text) + 1
      
      var_section <- substr(dict_text, var_start, next_var_start - 1)
      
      # Look for "Value label information for <varname>" in this section
      # Since striprtf already converts RTF to plain text, var_section is clean text
      # Format: "Value label information for shenv13\n\tValue = 0.0\tLabel = no\n\tValue = 1.0\tLabel = yes"
      value_label_header <- paste0("Value label information for ", var_df$variable[i])
      
      if (grepl(value_label_header, var_section)) {
        # Extract value-label pairs directly from var_section
        # Pattern: Value = <number> Label = <text>
        # Format in clean text: "\tValue = 0.0\tLabel = no" or "Value = 0.0\tLabel = no"
        value_pairs <- stringr::str_match_all(var_section, 
          "Value\\s*=\\s*([0-9.-]+)\\s+Label\\s*=\\s*([^\\n\\r]+)")[[1]]
        
        if (nrow(value_pairs) > 0) {
          # Clean up the extracted values and labels
          clean_values <- trimws(value_pairs[, 2])
          clean_labels <- trimws(value_pairs[, 3])
          
          # Remove any remaining whitespace or control characters
          clean_labels <- trimws(clean_labels)
          
          # Create data frame with value-label relationships
          # Each row represents one value-label pair (e.g., Value = 0.0, Label = no)
          value_labels_list[[var_df$variable[i]]] <- data.frame(
            variable = var_df$variable[i],
            value = clean_values,
            label = clean_labels,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  # Add value labels as a nested column (list of data frames)
  var_df$value_labels <- value_labels_list[var_df$variable]
  
  return(var_df)
}

# Helper function to get value labels for a specific variable
get_value_labels <- function(dictionary_df, var_name) {
  var_row <- dictionary_df[dictionary_df$variable == var_name, ]
  if (nrow(var_row) == 0) {
    return(NULL)
  }
  return(var_row$value_labels[[1]])
}

# Helper function to search variables by label
search_variables <- function(dictionary_df, pattern, ignore.case = TRUE) {
  matches <- grepl(pattern, dictionary_df$variable_label, ignore.case = ignore.case)
  return(dictionary_df[matches, ])
}

# Helper function to convert numeric values to human-readable labels
# Takes a data column and dictionary_expanded, returns the column with labels
convert_to_labels <- function(data_column, var_name, dictionary_expanded) {
  # Get value-label mapping for this variable
  var_labels <- dictionary_expanded[dictionary_expanded$variable == var_name & !is.na(dictionary_expanded$Value), ]
  
  if (nrow(var_labels) == 0) {
    # No labels available, return original column
    return(data_column)
  }
  
  # Create a mapping vector: value -> label
  # Handle both numeric and character values
  value_to_label <- setNames(var_labels$Label, as.character(var_labels$Value))
  
  # Convert data column to character for matching
  data_char <- as.character(data_column)
  
  # Replace values with labels
  # Keep original value if no match found (for NA or unmapped values)
  result <- ifelse(data_char %in% names(value_to_label), 
                   value_to_label[data_char], 
                   data_char)
  
  # Convert back to factor if original was factor, otherwise keep as character
  if (is.factor(data_column)) {
    return(factor(result, levels = unique(c(var_labels$Label, levels(data_column)))))
  } else {
    return(result)
  }
}

# ========================Command Parameters Setting=========================
option_list <- list(
  make_option(c("--input", "-i"),
              type = "character", default = "/data/gpfs/projects/punim2885/Xuan/Hackathon_2026_DT_LLM/Young Lives Data/Childhood survey_Rounds 1-5_Constructed files_2002-2016/UKDA-7483-tab/tab/vietnam_constructed.tab",
              help = "input file name"
  ), make_option(c("--output", "-o"),
                 type = "character", default = "Result/00_Data_Preprocessing_Readable",
                 help = "output directory path."
  ), make_option(c("--seed", "-s"),
              type = "integer", default = 9,
              help = "set.seed"
  ),
  make_option(c("--dictionary", "-d"),
              type = "character", default = "/data/gpfs/projects/punim2885/Xuan/Hackathon_2026_DT_LLM/Young Lives Data/Childhood survey_Rounds 1-5_Constructed files_2002-2016/UKDA-7483-tab/mrdoc/vietnam_constructed_ukda_data_dictionary.rtf",
              help = "dictionary file path"
  )
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
#============================================================================
if (is.null(opt$output)) {
  print("NO OUTPUT PATH SUPPLIED,current directory will be used!")
  output_dir <- getwd()
} else {
  output_dir <- opt$output
  if (!file.exists(output_dir)) {
    dir.create(output_dir, recursive = T)
  }
}

if (is.null(opt$seed)) {
  stop("Please provide the seed!")
}else{
  set.seed(opt$seed)
}

if (is.null(opt$input)) {
  stop("Please provide the input folder name!")
}else{
  input <- opt$input
  data <- read.table(input, header = TRUE, sep = "\t")
}

if (is.null(opt$dictionary)) {
  stop("Please provide the dictionary file path!")
}else{
  dictionary_path <- opt$dictionary  
  # Example usage of parsed dictionary:
  # View all variables: View(dictionary)
  # Get info for a specific variable: dictionary[dictionary$variable == "chsex", ]
  # Get value labels: get_value_labels(dictionary, "chsex")
  # Search variables: search_variables(dictionary, "health")
  # Access value labels directly: dictionary$value_labels[["chsex"]]
}
################################################################################
dictionary_raw <- striprtf::read_rtf(dictionary_path)
# Parse dictionary into structured data frame
dictionary <- parse_dictionary(dictionary_raw)
# Also keep raw text if needed
dictionary_raw_text <- dictionary_raw

# Create expanded dictionary with one row per value-label pair
# Structure: position, variable, variable_label, variable_type, spss_level, Value, Label
# If a variable has multiple value-label pairs, it will have multiple rows
dictionary_expanded <- do.call(rbind, lapply(seq_len(nrow(dictionary)), function(i) {
  var_info <- dictionary[i, ]
  
  # Get value labels for this variable
  value_labels_df <- dictionary$value_labels[[i]]
  
  # If variable has value labels, create one row per value-label pair
  if (!is.null(value_labels_df) && nrow(value_labels_df) > 0) {
    # Create a row for each value-label pair
    result <- data.frame(
      position = var_info$position,
      variable = var_info$variable,
      variable_label = var_info$variable_label,
      variable_type = var_info$variable_type,
      spss_level = var_info$spss_level,
      Value = value_labels_df$value,
      Label = value_labels_df$label,
      stringsAsFactors = FALSE
    )
  } else {
    # If no value labels, create one row with NA for Value and Label
    result <- data.frame(
      position = var_info$position,
      variable = var_info$variable,
      variable_label = var_info$variable_label,
      variable_type = var_info$variable_type,
      spss_level = var_info$spss_level,
      Value = NA_character_,
      Label = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  
  return(result)
}))

# Save the expanded dictionary to CSV
# Columns: position, variable, variable_label, variable_type, spss_level, Value, Label
# Example: inround will have 2 rows (one for Value=0.0 Label=no, one for Value=1.0 Label=yes)
# change dictionary_expanded$Value into integer
dictionary_expanded$Value <- as.integer(dictionary_expanded$Value)
write.csv(dictionary_expanded, paste0(output_dir, "/dictionary.csv"), row.names = FALSE)

# Also save full dictionary structure as RDS (preserves nested structure)
saveRDS(dictionary_expanded, paste0(output_dir, "/dictionary_expanded.rds"))

################################################################################
# Convert numeric values to human-readable labels in the data
# For variables that have value labels in the dictionary, replace numeric codes with labels
################################################################################

# Get list of variables that have value labels
variables_with_labels <- unique(dictionary_expanded$variable[!is.na(dictionary_expanded$Value)])

# Create a human-readable version of the data
data_readable <- data

# Convert each variable that has value labels
for (var_name in variables_with_labels) {
  if (var_name %in% names(data_readable)) {
    # Get value-label mapping for this variable
    var_labels <- dictionary_expanded[dictionary_expanded$variable == var_name & !is.na(dictionary_expanded$Value), ]
    
    if (nrow(var_labels) > 0) {
      # Create mapping: Value (integer) -> Label (character)
      # Since Value is already converted to integer in dictionary_expanded
      value_to_label <- setNames(var_labels$Label, var_labels$Value)
      
      # Get the data column
      data_col <- data_readable[[var_name]]
      
      # Try to convert to numeric for matching
      # Handle both integer and decimal values (e.g., 0, 0.0, 1, 1.0)
      data_values <- suppressWarnings(as.numeric(as.character(data_col)))
      
      # Check if column is numeric or can be converted to numeric
      # Skip if it's a character ID column (like childid = "VN010001")
      is_numeric_col <- is.numeric(data_col) || 
                       (!all(is.na(data_values) == is.na(data_col)) && 
                        sum(!is.na(data_values)) > 0)
      
      if (is_numeric_col) {
        # Column is numeric - replace values with labels
        # Convert data values to integer for matching (since dictionary Value is integer)
        data_values_int <- as.integer(data_values)
        
        # Create result vector
        result <- character(length(data_col))
        
        for (i in seq_along(data_col)) {
          if (is.na(data_values[i])) {
            result[i] <- NA_character_
          } else if (data_values_int[i] %in% var_labels$Value) {
            # Match found - use label
            result[i] <- var_labels$Label[var_labels$Value == data_values_int[i]][1]
          } else {
            # No match - keep original value as character
            result[i] <- as.character(data_col[i])
          }
        }
        
        data_readable[[var_name]] <- result
      }
      # If conversion failed (e.g., character IDs like "VN010001"), skip this variable
    }
  }
}

# if data_readable is na, change into "missing"
data_readable[is.na(data_readable)] <- "missing"

# Save the human-readable data
write.table(data_readable, paste0(output_dir, "/data_readable.tab"), 
            sep = "\t", row.names = FALSE, quote = FALSE)

# Also save as CSV for easier viewing
write.csv(data_readable, paste0(output_dir, "/data_readable.csv"), row.names = FALSE)
