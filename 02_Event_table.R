# conda activate /data/gpfs/projects/punim2885/Xuan/ENV/llm_dts_conda
#=========================Loading Packages===================================
suppressMessages(library("optparse"))
suppressMessages(library("tidyverse"))
#=========================Function Definition================================
# ========================Command Parameters Setting=========================
option_list <- list(
  make_option(c("--input", "-i"),
              type = "character", default = "Result/00_Data_Preprocessing_Readable_Vietnam_Childhood_survey_Rounds/data_readable.csv",
              help = "input file name"
  ), make_option(c("--output", "-o"),
                 type = "character", default = "Result/02_Event_table",
                 help = "output directory path."
  ), make_option(c("--seed", "-s"),
              type = "integer", default = 9,
              help = "set.seed"
  ),make_option(c("--dictionary", "-d"),
              type = "character", default = "Result/00_Data_Preprocessing_Readable_Vietnam_Childhood_survey_Rounds/dictionary.csv",
              help = "dictionary file path"
  ),make_option(c("--meta_data", "-m"),
              type = "character", default = "Hackathon_2026_DT_LLM/Young Lives Data/Childhood survey_Rounds 1-5_Constructed files_2002-2016/UKDA-7483-tab/tab/vietnam_constructed.tab",
              help = "info for meta_data column"
  ),
  make_option(c("--constants", "-c"),
              type = "character", default = "Result/01_Data_Preparation_constants",
              help = "constants file path"
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
  data <- read.csv(input, header = TRUE, sep = ",")
}

if (is.null(opt$dictionary)) {
  stop("Please provide the dictionary file name!")
}else{
  dictionary <- opt$dictionary
  dictionary <- read.csv(dictionary, header = TRUE, sep = ",")
}

if (is.null(opt$meta_data)) {
  stop("Please provide the meta data file name!")
}else{
  meta_data <- opt$meta_data
}

if (is.null(opt$constants)) {
  stop("Please provide the constants file name!")
}else{
  constants <- read.csv(file.path(opt$constants, "constants.csv"), header = TRUE, sep = ",")
  constants_description <- read.csv(file.path(opt$constants, "constants_description.csv"), header = TRUE, sep = ",")
}

#=========================Main Function=========================================
data_formatted = data %>% dplyr::select(childid, dint, everything()) %>% rename(patientid = childid, date = dint)

# Create a lookup table for variable labels (one per variable)
var_label_lookup <- dictionary %>%
  dplyr::select(variable, variable_label) %>%
  distinct(variable, .keep_all = TRUE)

# Convert all columns (except patientid and date) to character before pivoting
# This is necessary because pivot_longer can't combine columns of different types
data_formatted_char <- data_formatted %>%
  mutate(across(-c(patientid, date), as.character))

# Pivot data from wide to long format
events_long <- data_formatted_char %>%
  pivot_longer(
    cols = -c(patientid, date),
    names_to = "event_name",
    values_to = "event_value"
  ) %>%
  # Remove rows with missing values
  filter(!is.na(event_value) & 
         event_value != "missing" & 
         event_value != "" &
         event_value != "NA")

# Join with dictionary to get event_descriptive_name
events_table <- events_long %>%
  left_join(var_label_lookup, by = c("event_name" = "variable")) %>%
  # Create the final events table structure
  mutate(
    event_descriptive_name = ifelse(is.na(variable_label) | variable_label == "", 
                                    event_name, 
                                    variable_label),
    event_category = NA_character_,  # Optional - can be populated later
    meta_data = meta_data,  # Optional - can be populated later
    source = NA_character_  # Optional
  ) %>%
  # Select and reorder columns according to the schema
  dplyr::select(
    patientid,
    date,
    event_descriptive_name,
    event_category,
    event_name,
    event_value,
    meta_data,
    source
  )

# Add meta_data column
if (!is.null(meta_data)) {
  events_table <- events_table %>%
    mutate(meta_data = meta_data)
}

# remove event_value that is in constants
events_table <- events_table %>%
  filter(!event_name %in% colnames(constants))

# add event_category column
events_table <- events_table %>%
  mutate(event_category = ifelse(event_value %in% constants$value, constants$category, NA))

# sort
events_table <- events_table %>%
  dplyr::select(patientid, date, event_descriptive_name,event_value, everything())

# if any column in events_table is all na, remove the column
if (any(apply(events_table, 2, function(x) all(is.na(x))))) {
  events_table <- events_table %>%
    dplyr::select(-which(apply(events_table, 2, function(x) all(is.na(x)))))
}

# Save the events table
write.csv(events_table, 
          file = file.path(output_dir, "events.csv"),
          row.names = FALSE,
          na = "")

cat("Events table created successfully!\n")
cat("Output saved to:", file.path(output_dir, "events.csv"), "\n")
cat("Total events:", nrow(events_table), "\n")

