### Environment
# getwd()
# setwd("")
# Sys.getenv("VAR1")
# rm(list = ls())  # clean global env
# install.packages("")

# cat(paste(vector, collapse = "\n"))

rm(list = ls()) # clean global env

library(tidyverse)
library(openxlsx)
library(readxl)


### CONVERT

# heval3xls <- read_excel(
#     path = "data/raw/Human_Evaluation_Results_3rd.xlsx",
#     sheet = 1, # Sheet number
#     col_names = TRUE,
#     na = "" # NA handling
# )
# write.csv(heval3xls,
#     file = "data/raw/Human_Evaluation_Results_3rd.csv",
#     row.names = FALSE,
#     quote = TRUE, # Quote strings, factors, and character vectors
#     na = "", # How to represent NA values
#     fileEncoding = "UTF-8"
# )
# rm(heval3xls)


### IMPORT

heval3 <- read.csv(
    "data/raw/Human_Evaluation_Results_3rd.csv",
    header = TRUE, # without headers
    sep = ",",
    na.strings = c("", "NA"),
    fill = TRUE, # fill empty cells with NA
    stringsAsFactors = FALSE
)
heval12 <- read.csv(
    "data/processed/MASTER_human-eval.csv", # 1st+2nd round
    header = TRUE, # without headers
    sep = ",",
    na.strings = c("", "NA"),
    fill = TRUE, # fill empty cells with NA
    stringsAsFactors = FALSE
)
meval <- read.csv(
    "data/processed/MASTER_machine-eval.csv",
    header = TRUE, # without headers
    sep = ",",
    na.strings = c("", "NA"),
    fill = TRUE, # fill empty cells with NA
    stringsAsFactors = FALSE
)

heval3 <- heval3 %>%
    # Convert to int
    mutate(smm_consistency = as.integer(smm_consistency)) %>%
    # Drop non-eligible
    filter(
        (smm_consistency == 0 | smm_consistency == 1) &
            !is.na(datapoint)
    )
heval3_long <- heval3 # keep original/long data

### CLEANING

# Handle NA

# Cols

heval3_names <- colnames(heval3_long)
heval3_labels <- c(
    "Source",
    "Evaluator name",
    "Dataset",
    "Datapoint",
    "Summary: Consistency",
    "Article: Coalition Breakdown",
    "Article: SPD",
    "Article: FDP",
    "Article: Greens",
    "Article: Scholz",
    "Article: Lindner",
    "Article: Habeck",
    "Summary: Coalition Breakdown",
    "Summary: SPD",
    "Summary: FDP",
    "Summary: Greens",
    "Summary: Scholz",
    "Summary: Lindner",
    "Summary: Habeck",
    "Comment",
    "Datapoint_new"
)

# Rows


### MANIPULATE DATA

# Colnames to df
#   colnames(heval3) <- heval3_names

# Change data


### IDENTIFY DUPLICATES

# Keep first occurrence of duplicates in original df
heval3 <- heval3_long %>%
    filter(!duplicated(datapoint_new))

# Copy subsequent duplicates to new df
heval3_duplicates <- heval3_long %>%
    filter(duplicated(datapoint_new))

# Substract all duplicates from original df
# because 1 validator observation is enough
heval3_short <- heval3 %>%
    anti_join(heval3_duplicates, by = "datapoint_new") # Only use for allocation, nothing else!

### ALLOCATE

# Save duplicates ...
# preprocess_3rd_allocate.R


### MASTER datasets
#   Combine heval, meval

heval <- bind_rows(heval12, heval3)

master <- bind_rows(heval, meval)

write.xlsx(master,
    file = "data/processed/3rd/MASTER_combined_3rd.xlsx",
    sheetName = "master",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.csv(master,
    file = "data/processed/3rd/MASTER_combined_3rd.csv",
    row.names = FALSE,
    quote = TRUE, # Quote strings, factors, and character vectors
    na = "", # How to represent NA values
    fileEncoding = "UTF-8"
)

write.xlsx(heval,
    file = "data/processed/3rd/MASTER_human-eval_3rd.xlsx",
    sheetName = "heval",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.csv(heval,
    file = "data/processed/3rd/MASTER_human-eval_3rd.csv",
    row.names = FALSE,
    quote = TRUE, # Quote strings, factors, and character vectors
    na = "", # How to represent NA values
    fileEncoding = "UTF-8"
)
