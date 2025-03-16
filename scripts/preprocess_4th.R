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

# heval4xls <- read_excel(
#     path = "data/raw/Human_Evaluation_Results_4th.xlsx",
#     sheet = 1, # Sheet number
#     col_names = TRUE,
#     na = "" # NA handling
# )
# write.csv(heval4xls,
#     file = "data/raw/Human_Evaluation_Results_4th.csv",
#     row.names = FALSE,
#     quote = TRUE, # Quote strings, factors, and character vectors
#     na = "", # How to represent NA values
#     fileEncoding = "UTF-8"
# )
# rm(heval4xls)


### IMPORT

heval4 <- read.csv(
    "data/raw/Human_Evaluation_Results_4th.csv",
    header = TRUE, # without headers
    sep = ",",
    na.strings = c("", "NA"),
    fill = TRUE, # fill empty cells with NA
    stringsAsFactors = FALSE
)
heval123 <- read.csv(
    "data/processed/3rd/MASTER_human-eval_3rd.csv", # 1st+2nd+3rd round
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

heval4 <- heval4 %>%
    # Convert to int
    mutate(
        atc_coalition = as.character(atc_coalition),
        smm_consistency = as.integer(smm_consistency)
    ) %>%
    # Drop non-eligible
    filter(
        (smm_consistency == 0 | smm_consistency == 1) &
            !is.na(datapoint)
    )
heval4_long <- heval4 # keep original/long data

### CLEANING

# Handle NA

# Cols

heval4_names <- colnames(heval4_long)
heval4_labels <- c(
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
#   colnames(heval4) <- heval4_names

# Change data


### IDENTIFY DUPLICATES

# Keep first occurrence of duplicates in original df
heval4 <- heval4_long %>%
    filter(!duplicated(datapoint_new))

# Copy subsequent duplicates to new df
heval4_duplicates <- heval4_long %>%
    filter(duplicated(datapoint_new))

# Substract all duplicates from original df
# because 1 validator observation is enough
heval4_short <- heval4 %>%
    anti_join(heval4_duplicates, by = "datapoint_new") # Only use for allocation, nothing else!

### ALLOCATE

# Save duplicates ...
# ..


### MASTER datasets
#   Combine heval, meval

heval <- bind_rows(heval123, heval4)

master <- bind_rows(heval, meval)

write.xlsx(master,
    file = "data/processed/4th/MASTER_combined_4th.xlsx",
    sheetName = "master",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.csv(master,
    file = "data/processed/4th/MASTER_combined_4th.csv",
    row.names = FALSE,
    quote = TRUE, # Quote strings, factors, and character vectors
    na = "", # How to represent NA values
    fileEncoding = "UTF-8"
)

write.xlsx(heval,
    file = "data/processed/4th/MASTER_human-eval_4th.xlsx",
    sheetName = "heval",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.csv(heval,
    file = "data/processed/4th/MASTER_human-eval_4th.csv",
    row.names = FALSE,
    quote = TRUE, # Quote strings, factors, and character vectors
    na = "", # How to represent NA values
    fileEncoding = "UTF-8"
)
