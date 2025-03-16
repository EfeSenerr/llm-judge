### Environment
# getwd()
# setwd("")
# Sys.getenv("VAR1")
# rm(list = ls())  # clean global env
# install.packages("")

# cat(paste(vector, collapse = "\n"))

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

heval3_long <- heval3 # keep original

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


### CLEANING

# Handle NA

# Cols

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

# Save duplicates ...
# preprocess_3rd_allocate.R
