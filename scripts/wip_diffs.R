### Environment
# getwd()
# setwd("")
# Sys.getenv("VAR1")
# rm(list = ls())  # clean global env
# install.packages("")

### GHC prompt:
# Let's code in R today. My dataset is called df. I have imported tidyverse and want to use it whenever possible. Do not repeat the content of my file unless I ask you to.

### Add Diffs to datasets

library(tidyverse)
library(openxlsx)

inputfile <- "data/raw/evaluation_results_aggregated_0503.csv" # LLM-evals
# inputfilename <- basename(inputfile)  # incl. suffix
inputfilename <- tools::file_path_sans_ext(basename(inputfile)) # excl. suffix

df <- read.csv(inputfile)
glimpse(df)

# Rename cols
df <- df %>%
    rename(
        diff_scholz = Comparison_Olaf_Scholz,
        diff_spd = Comparison_SPD,
        diff_habeck = Comparison_Robert_Habeck,
        diff_gruene = Comparison_die_Gruenen,
        diff_lindner = Comparison_Christian_Lindner,
        diff_fdp = Comparison_FDP,
        diff_coalition = Comparison_the_coalition_breakdown
    )

# Clear all values from col starting with "diff"
df <- df %>%
    mutate_at(vars(starts_with("diff_")), ~NA)

# Calculate new values for diff_* with proper NA handling
df <- df %>%
    mutate(
        diff_scholz = case_when(
            is.na(Article_Olaf_Scholz) & is.na(Summary_Olaf_Scholz) ~ 1,
            is.na(Article_Olaf_Scholz) | is.na(Summary_Olaf_Scholz) ~ 0,
            Article_Olaf_Scholz == Summary_Olaf_Scholz ~ 1,
            TRUE ~ 0
        ),
        diff_spd = case_when(
            is.na(Article_SPD) & is.na(Summary_SPD) ~ 1,
            is.na(Article_SPD) | is.na(Summary_SPD) ~ 0,
            Article_SPD == Summary_SPD ~ 1,
            TRUE ~ 0
        ),
        diff_habeck = case_when(
            is.na(Article_Robert_Habeck) & is.na(Summary_Robert_Habeck) ~ 1,
            is.na(Article_Robert_Habeck) | is.na(Summary_Robert_Habeck) ~ 0,
            Article_Robert_Habeck == Summary_Robert_Habeck ~ 1,
            TRUE ~ 0
        ),
        diff_gruene = case_when(
            is.na(Article_die_Gruenen) & is.na(Summary_die_Gruenen) ~ 1,
            is.na(Article_die_Gruenen) | is.na(Summary_die_Gruenen) ~ 0,
            Article_die_Gruenen == Summary_die_Gruenen ~ 1,
            TRUE ~ 0
        ),
        diff_lindner = case_when(
            is.na(Article_Christian_Lindner) & is.na(Summary_Christian_Lindner) ~ 1,
            is.na(Article_Christian_Lindner) | is.na(Summary_Christian_Lindner) ~ 0,
            Article_Christian_Lindner == Summary_Christian_Lindner ~ 1,
            TRUE ~ 0
        ),
        diff_fdp = case_when(
            is.na(Article_FDP) & is.na(Summary_FDP) ~ 1,
            is.na(Article_FDP) | is.na(Summary_FDP) ~ 0,
            Article_FDP == Summary_FDP ~ 1,
            TRUE ~ 0
        ),
        diff_coalition = case_when(
            is.na(Article_the_coalition_breakdown) & is.na(Summary_the_coalition_breakdown) ~ 1,
            is.na(Article_the_coalition_breakdown) | is.na(Summary_the_coalition_breakdown) ~ 0,
            Article_the_coalition_breakdown == Summary_the_coalition_breakdown ~ 1,
            TRUE ~ 0
        )
    )

outputfile <- paste0("data/processed/", inputfilename, "_DIFF", ".xlsx")
write.xlsx(df, outputfile, sheetName = "Sheet1")

outputfile <- paste0("data/processed/", inputfilename, "_DIFF", ".csv")
write.csv(df,
    file = outputfile,
    row.names = FALSE,
    quote = TRUE, # Quote strings, factors, and character vectors
    na = "", # How to represent NA values
    fileEncoding = "UTF-8"
)

#######################

library(tidyverse)
library(openxlsx)

inputfile <- "data/processed/human_eval_combined_MASTER.csv" # Human-evals
# inputfilename <- basename(inputfile)  # incl. suffix
inputfilename <- tools::file_path_sans_ext(basename(inputfile)) # excl. suffix

df <- read.csv(inputfile)
glimpse(df)

# Add cols
df <- df %>%
    mutate(
        diff_scholz = "N/A",
        diff_spd = "N/A",
        diff_habeck = "N/A",
        diff_gruene = "N/A",
        diff_lindner = "N/A",
        diff_fdp = "N/A",
        diff_coalition = "N/A"
    )

# Clear all values from col starting with "diff"
df <- df %>%
    mutate_at(vars(starts_with("diff_")), ~"N/A")

# Calculate new values for diff_* with proper NA handling
## ATTENTION - error in naming:
## LLM:
##  Summary_the_coalition_breakdown  # OK
##  Article_the_coalition_breakdown  # OK
## Human:
##  Article_the_coalition_breakdown  # OK
##  Summary_the_coalition_breackdown  # !
df <- df %>%
    mutate(
        diff_scholz = case_when(
            is.na(Article_Olaf_Scholz) & is.na(Summary_Olaf_Scholz) ~ 1,
            is.na(Article_Olaf_Scholz) | is.na(Summary_Olaf_Scholz) ~ 0,
            Article_Olaf_Scholz == Summary_Olaf_Scholz ~ 1,
            TRUE ~ 0
        ),
        diff_spd = case_when(
            is.na(Article_SPD) & is.na(Summary_SPD) ~ 1,
            is.na(Article_SPD) | is.na(Summary_SPD) ~ 0,
            Article_SPD == Summary_SPD ~ 1,
            TRUE ~ 0
        ),
        diff_habeck = case_when(
            is.na(Article_Robert_Habeck) & is.na(Summary_Robert_Habeck) ~ 1,
            is.na(Article_Robert_Habeck) | is.na(Summary_Robert_Habeck) ~ 0,
            Article_Robert_Habeck == Summary_Robert_Habeck ~ 1,
            TRUE ~ 0
        ),
        diff_gruene = case_when(
            is.na(Article_die_Gruenen) & is.na(Summary_die_Gruenen) ~ 1,
            is.na(Article_die_Gruenen) | is.na(Summary_die_Gruenen) ~ 0,
            Article_die_Gruenen == Summary_die_Gruenen ~ 1,
            TRUE ~ 0
        ),
        diff_lindner = case_when(
            is.na(Article_Christian_Lindner) & is.na(Summary_Christian_Lindner) ~ 1,
            is.na(Article_Christian_Lindner) | is.na(Summary_Christian_Lindner) ~ 0,
            Article_Christian_Lindner == Summary_Christian_Lindner ~ 1,
            TRUE ~ 0
        ),
        diff_fdp = case_when(
            is.na(Article_FDP) & is.na(Summary_FDP) ~ 1,
            is.na(Article_FDP) | is.na(Summary_FDP) ~ 0,
            Article_FDP == Summary_FDP ~ 1,
            TRUE ~ 0
        ),
        diff_coalition = case_when(
            is.na(Article_the_coalition_breakdown) & is.na(Summary_the_coalition_breackdown) ~ 1,
            is.na(Article_the_coalition_breakdown) | is.na(Summary_the_coalition_breackdown) ~ 0,
            Article_the_coalition_breakdown == Summary_the_coalition_breackdown ~ 1,
            TRUE ~ 0
        )
    )

outputfile <- paste0("data/processed/", inputfilename, "_DIFF", ".xlsx")
write.xlsx(df, outputfile, sheetName = "Sheet1")

outputfile <- paste0("data/processed/", inputfilename, "_DIFF", ".csv")
write.csv(df,
    file = outputfile,
    row.names = FALSE,
    quote = TRUE,
    na = "", # Has been formatted as "N/A" before so no command necessary
    fileEncoding = "UTF-8"
)
