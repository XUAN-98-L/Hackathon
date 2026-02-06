# conda activate /data/gpfs/projects/punim2885/Xuan/ENV/llm_dts_conda
#=========================Loading Packages===================================
suppressMessages(library("dplyr"))
suppressMessages(library("optparse"))
suppressMessages(library("tidyverse"))
suppressMessages(library("striprtf"))
suppressMessages(library("stringr"))
# ========================Command Parameters Setting=========================
option_list <- list(
  make_option(c("--input", "-i"),
              type = "character", default = "Result/00_Data_Preprocessing_Readable_Vietnam_Childhood_survey_Rounds",
              help = "input file name"
  ), make_option(c("--output", "-o"),
                 type = "character", default = "Result/01_Data_Preparation_constants",
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
  input_path <- opt$input
}



################################################################################

data_readable <- read_csv(paste0(input_path, "/data_readable.csv"))
dictionary <- read_csv(paste0(input_path, "/dictionary.csv"))


## constant --------------------------------------------------------------------

constant_vars <- data_readable %>%
  group_by(childid) %>%
  summarise(across(everything(),
                   ~ n_distinct(.x, na.rm = TRUE))) %>%
  summarise(across(everything(),
                   ~ max(.x, na.rm = TRUE))) %>%
  as.list() %>%
  unlist() %>%
  {names(.)[. <= 1]}

constant_vars

constants <- data_readable %>%
  arrange(childid, round) %>%
  group_by(childid) %>%
  summarise(across(all_of(constant_vars),
                   ~ first(na.omit(.x))),
            .groups = "drop")


## constant description --------------------------------------------------------

constant_description <- tibble(
  variable = colnames(constants)
) %>%
  mutate(
    comment = dictionary$variable_label[
      match(variable, dictionary$variable)
    ]
  )


# SAVE =========================================================================

# SAVE CONSTANTS:
# if constants is na, change into "missing"
constants[is.na(constants)] <- "missing"
# rename childid id to patientid
constants <- constants %>%
  rename(patientid = childid)
# Also as CSV for easier viewing
write.csv(constants, paste0(output_dir, "/constants.csv"), row.names = FALSE)

# SAVE CONSTANTS DESCRIPTIONS
# Also as CSV for easier viewing
write.csv(constant_description, paste0(output_dir, "/constants_description.csv"), row.names = FALSE)

