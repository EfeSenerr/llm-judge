### Environment
# getwd()
# setwd("")
# Sys.getenv("VAR1")
# rm(list = ls())  # clean global env
# install.packages("")

### Add Diffs to datasets

library(tidyverse)
library(openxlsx)

inputfile <- "data/processed/MASTER_combined.csv"
# inputfile <- "data/processed/MASTER_human-eval.csv"
# inputfile <- "data/processed/MASTER_machine-eval.csv"
#
# inputfile <- "data/processed/3rd/MASTER_combined_3rd.csv"
# inputfile <- "data/processed/3rd/MASTER_human-eval_3rd.csv"

# inputfilename <- basename(inputfile)  # incl. suffix
inputfilename <- tools::file_path_sans_ext(basename(inputfile)) # excl. suffix
df <- read.csv(inputfile)

# Drop cols
# df <- df %>%
# select(atc_scholz, smm_scholz)

# Colnames to vector
df_names <- names(df) %>%
    str_subset("^(atc_|smm_)") %>% # subset cols
    setdiff("smm_consistency") # exclude col

# Calc directional diff
#   -1 > 1 = 2
#   0 > NONE = NA
df <- df %>%
    mutate(diff_coalition = case_when(
        atc_coalition == "NONE" | smm_coalition == "NONE" ~ NA,
        TRUE ~ as.character(as.numeric(smm_coalition) - as.numeric(atc_coalition))
    )) %>%
    mutate(diff_spd = case_when(
        atc_spd == "NONE" | smm_spd == "NONE" ~ NA,
        TRUE ~ as.character(as.numeric(smm_spd) - as.numeric(atc_spd))
    )) %>%
    mutate(diff_fdp = case_when(
        atc_fdp == "NONE" | smm_fdp == "NONE" ~ NA,
        TRUE ~ as.character(as.numeric(smm_fdp) - as.numeric(atc_fdp))
    )) %>%
    mutate(diff_green = case_when(
        atc_green == "NONE" | smm_green == "NONE" ~ NA,
        TRUE ~ as.character(as.numeric(smm_green) - as.numeric(atc_green))
    )) %>%
    mutate(diff_scholz = case_when(
        atc_scholz == "NONE" | smm_scholz == "NONE" ~ NA,
        TRUE ~ as.character(as.numeric(smm_scholz) - as.numeric(atc_scholz))
    )) %>%
    mutate(diff_lindner = case_when(
        atc_lindner == "NONE" | smm_lindner == "NONE" ~ NA,
        TRUE ~ as.character(as.numeric(smm_lindner) - as.numeric(atc_lindner))
    )) %>%
    mutate(diff_habeck = case_when(
        atc_habeck == "NONE" | smm_habeck == "NONE" ~ NA,
        TRUE ~ as.character(as.numeric(smm_habeck) - as.numeric(atc_habeck))
    ))


# Check for NAs
glimpse(df)
sum(is.na(df$atc_scholz))
sum(is.na(df$smm_scholz))
sum(is.na(df$diff_scholz))

### SAVE FILE

outputfile <- paste0("data/processed/", inputfilename, "_DIFFS", ".xlsx")
write.xlsx(df,
    outputfile,
    sheetName = "diffs",
    rowNames = FALSE,
    colNames = TRUE,
    asTable = FALSE,
    na.string = "",
    overwrite = TRUE
)

outputfile <- paste0("data/processed/", inputfilename, "_DIFFS", ".csv")
write.csv(df,
    file = outputfile,
    row.names = FALSE,
    quote = TRUE, # Quote strings, factors, and character vectors
    na = "", # How to represent NA values
    fileEncoding = "UTF-8"
)

rm(list = ls()) # clean global env
