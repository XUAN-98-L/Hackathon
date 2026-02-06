# conda activate /data/gpfs/projects/punim2885/Xuan/ENV/llm_dts_conda
#=========================Loading Packages===================================
suppressMessages(library("optparse"))
suppressMessages(library("tidyverse"))
#=========================Function Definition================================
# ========================Command Parameters Setting=========================
option_list <- list(
  make_option(c("--input", "-i"),
              type = "character", default = "model_data",
              help = "input file name"
  ), make_option(c("--output", "-o"),
                 type = "character", default = "model_data/Demo",
                 help = "output directory path."
  ), make_option(c("--seed", "-s"),
              type = "integer", default = 9,
              help = "set.seed"
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
  constants_description <- read.csv(file.path(input, "constants_description.csv"), header = TRUE, sep = ",")
  constants <- read.csv(file.path(input, "constants.csv"), header = TRUE, sep = ",")
  events <- read.csv(file.path(input, "events.csv"), header = TRUE, sep = ",")
}

#=========================Main Function=========================================
# numbers of each envents
event_counts <- events %>%
  group_by(event_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# select the top 3, then least 3 and random 3
top_1 <- event_counts %>%
  head(1)
least_1 <- event_counts %>%
  tail(1)
random_1 <- event_counts %>%
  sample_n(1)

# combine the data
demo_data <- bind_rows(top_1, least_1, random_1)

# filter the events
demo_events <- events %>%
  filter(event_name %in% demo_data$event_name)

# convert demo_events$date to date format
# Handle dates that are already in Date format or character strings
if (!inherits(demo_events$date, "Date")) {
  # Try to convert from M/D/YYYY format
  # Remove any leading/trailing whitespace first
  demo_events$date <- trimws(as.character(demo_events$date))
  # Convert to Date, handling NA values
  demo_events$date <- as.Date(demo_events$date, format = "%m/%d/%Y")
}

# write constants_description, constants, events
write.csv(constants_description, file.path(output_dir, "constants_description.csv"), row.names = FALSE)
write.csv(constants, file.path(output_dir, "constants.csv"), row.names = FALSE)
write.csv(demo_events, file.path(output_dir, "events.csv"), row.names = FALSE)