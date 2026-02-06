# conda activate /data/gpfs/projects/punim2885/Xuan/ENV/llm_dts_conda
#=========================Loading Packages===================================
suppressMessages(library("optparse"))
suppressMessages(library("tidyverse"))
#=========================Function Definition================================
# ========================Command Parameters Setting=========================
option_list <- list(
  make_option(c("--events", "-e"),
              type = "character", default = "Result/02_Event_table/events.csv",
              help = "events file path"
  ), make_option(c("--output", "-o"),
                 type = "character", default = "Result/03_Demo_data",
                 help = "output directory path."
  ), make_option(c("--constants", "-c"),
              type = "character", default = "Result/01_Data_Preparation_constants",
              help = "constants file path"
  ),
  make_option(c("--seed", "-s"),
              type = "integer", default = 9,
              help = "set.seed"
  ), make_option(c("--patient_subset", "-t"),type = "logical", default = FALSE,
              help = "subset the patients"
  ), make_option(c("--patient_proportion", "-p"),
              type = "numeric", default = NULL,
              help = "proportion of patients"
  ), make_option(c("--event_proportion", "-e"),
              type = "numeric", default = 0.005,
              help = "proportion of events"
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

if (is.null(opt$constants)) {
  stop("Please provide the constants file name!")
}else{
  constants <- read.csv(file.path(opt$constants, "constants.csv"), header = TRUE, sep = ",")
  constants_description <- read.csv(file.path(opt$constants, "constants_description.csv"), header = TRUE, sep = ",")
}

if (is.null(opt$events)) {
  stop("Please provide the events file name!")
}else{
  events <- read.csv(opt$events, header = TRUE, sep = ",")
}

if (is.null(opt$seed)) {
  stop("Please provide the seed!")
}else{
  set.seed(opt$seed)
}

if (is.null(opt$patient_subset)) {
  stop("Please provide the subset!")
}else{
  patient_subset <- opt$patient_subset
  patient_proportion <- opt$patient_proportion
}

if (is.null(opt$event_proportion)) {
  stop("Please provide the event proportion!")
}else{
  event_proportion <- opt$event_proportion
}
#=========================Main Function=========================================
# numbers of each envents
event_counts <- events %>%
  group_by(event_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# subset the events 
event_counts$count <- round(event_counts$count * event_proportion)
# keep only event_names with count > 0
event_counts <- event_counts %>% filter(count > 0)

# based on event_counts$event_name and event_counts$count, filter the events
# For each event_name, sample up to count rows (slice_sample requires constant n, so use group_map)
demo_events <- events %>%
  inner_join(event_counts, by = "event_name") %>%
  group_by(event_name) %>%
  group_map(~ slice_sample(.x, n = min(nrow(.x), .x$count[1]), replace = FALSE), .keep = TRUE) %>%
  bind_rows() %>%
  dplyr::select(-count)

# subset the constants
constants <- constants %>% filter(patientid %in% demo_events$patientid)

# convert demo_events$date to date format
# Handle dates that are already in Date format or character strings
if (!inherits(demo_events$date, "Date")) {
  # Try to convert from M/D/YYYY format
  # Remove any leading/trailing whitespace first
  demo_events$date <- trimws(as.character(demo_events$date))
  # Convert to Date, handling NA values
  demo_events$date <- as.Date(demo_events$date, format = "%m/%d/%Y")
}

if (patient_subset && !is.null(patient_proportion)) {
  patient_counts <- constants %>%
    distinct(patientid) %>%
    summarise(count = n())
  patient_count <- round(patient_counts$count * patient_proportion)
  # randemply select the patients
  patient_ids <- constants %>%
    distinct(patientid) %>%
    sample_n(patient_count)
  # filter constant
  constants <- constants %>%
    filter(patientid %in% patient_ids$patientid)
  # filter demo_events
  demo_events <- demo_events %>%
    filter(patientid %in% patient_ids$patientid)
}  

# write constants_description, constants, events
write.csv(constants_description, file.path(output_dir, "constants_description.csv"), row.names = FALSE)
write.csv(constants, file.path(output_dir, "constants.csv"), row.names = FALSE)
write.csv(demo_events, file.path(output_dir, "events.csv"), row.names = FALSE)