### Environment
# getwd()
# setwd("")
# Sys.getenv("VAR1")
# rm(list = ls())  # clean global env
# install.packages("")

# cat(paste(vector, collapse = "\n"))

library(tidyverse)
library(openxlsx)
# library(ggplot2)
# library(svglite)

### IMPORT

heval1 <- read.csv(
    "data/raw/Human_Evaluation_Results.csv",
    header = FALSE, # without headers
    sep = ",",
    na.strings = c("", "NA"),
    fill = TRUE, # fill empty cells with NA
    stringsAsFactors = FALSE
)
heval2 <- read.csv(
    "data/raw/Human_Evaluation_Results_differences.csv",
    header = FALSE, # without headers
    sep = ",",
    fill = TRUE, # fill empty cells with NA
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE
)
meval <- read.csv(
    "data/raw/evaluation_results_aggregated_1303.csv",
    header = TRUE, # incl headers
    sep = ",",
    fill = TRUE, # fill empty cells with NA
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE
)

# head(df)
# colnames(df)
# View(head(df))


### CLEANING
#   Eval: Llama (evaluation_results_aggregated_0503.csv)

# Handle NA
# Attention: Handled differently than HumanEval dataset!
#   OLD PROMPT
meval[is.na(meval)] <- "NONE" # replace all
#   NEW PROMPT
# meval[is.na(meval)] <- NA

# Cols

# harmonize col names
meval <- meval %>%
    rename(
        id = ID,
        dataset = Article.File.Name,
        datapoint = Article_Number,
        title = Article_Name,
        smm_consistency = Summary_Consistency,
        atc_scholz = Article_Olaf_Scholz,
        atc_spd = Article_SPD,
        atc_habeck = Article_Robert_Habeck,
        atc_green = Article_die_Gruenen,
        atc_lindner = Article_Christian_Lindner,
        atc_fdp = Article_FDP,
        atc_coalition = Article_the_coalition_breakdown,
        smm_scholz = Summary_Olaf_Scholz,
        smm_spd = Summary_SPD,
        smm_habeck = Summary_Robert_Habeck,
        smm_green = Summary_die_Gruenen,
        smm_lindner = Summary_Christian_Lindner,
        smm_fdp = Summary_FDP,
        smm_coalition = Summary_the_coalition_breakdown
    )

# Drop cols
meval <- meval %>%
    select(-id, -title) %>% # not used in heval1,2
    select(-c(
        "Comparison_Olaf_Scholz",
        "Comparison_SPD",
        "Comparison_Robert_Habeck",
        "Comparison_die_Gruenen",
        "Comparison_Christian_Lindner",
        "Comparison_FDP",
        "Comparison_the_coalition_breakdown"
    ))

# add identifiers
meval <- meval %>%
    mutate(
        source = "meval", # source file
        evaluator = "Llama" # evaluator name
    ) %>%
    select(source, evaluator, everything()) # move cols to front

# Rows
meval <- meval %>%
    # slice(-1) # drop 1st empty
    # Drop non-eligible: consistency=NONE *OR* datapoint=NONE
    filter(smm_consistency != "NONE" & datapoint != "NONE")

### MANIPULATE DATA

# Combine cols (dataset & datapoint)
meval <- meval %>%
    mutate(datapoint_new = paste(dataset, datapoint, sep = "_"))



### CLEANING
#   Eval: Human 1 (Human_Evaluation_Results.csv)

# Handle NA
# Attention: Handled differently than MachineEval dataset!
#   OLD PROMPT
heval1[heval1 == "N/A"] <- "NONE" # replace all "N/A" > "NONE"

# Cols

# harmonize col names
heval1_names <- c(
    "source",
    "evaluator",
    # "id", # only in 2nd dataset "heval2"
    "dataset",
    "datapoint",
    # "title", only in meval & heval2, not used in heval1
    "smm_consistency",
    "atc_coalition",
    "atc_spd",
    "atc_fdp",
    "atc_green",
    "atc_scholz",
    "atc_lindner",
    "atc_habeck",
    "smm_coalition",
    "smm_spd",
    "smm_fdp",
    "smm_green",
    "smm_scholz",
    "smm_lindner",
    "smm_habeck",
    "comment"
)

# add identifiers ## Must come *before* colnames assignment!
heval1 <- heval1 %>%
    mutate(source = "heval1") %>% # source file
    select(source, everything()) # move cols to front

# colnames to df
colnames(heval1) <- heval1_names

# Change data
heval1 <- heval1 %>% # fix dataset name
    mutate(dataset = dataset %>%
        str_replace("ampel_aus_0", "newspaper_ampel-aus_0-200_processed.json") %>%
        str_replace("DDay", "newspaper_D-Day_processed.json") %>%
        str_replace("articles_newswires", "articles_newswires_processed.json") %>%
        str_replace("articles_newspapers", "articles_newspapers_processed.json") %>%
        str_replace("ampel_aus_201", "newspaper_ampel-aus_201-300_processed.json") %>%
        str_replace("ampel_aus_300", "newspaper_ampel-aus_300-400_processed.json") %>%
        str_replace("ampel_koalition", "newspaper_Ampel_Koalition am Ende_52_processed.json") %>%
        str_replace("koalitionskrise", "newspaper_koalitionskrise_52_processed.json"))

heval1 <- heval1 %>% # fix evaluator naming
    mutate(evaluator = if_else(evaluator == "Charly", "Charlotte", evaluator))

# Rows
heval1 <- heval1 %>%
    slice(-(1:2)) # drop 1:2

heval1 <- heval1 %>% # Drop non-eligible: consistency=NONE *OR* datapoint=NONE
    filter(smm_consistency != "NONE" & datapoint != "NONE")

# Clean
rm(heval1_names)


### CLEANING
#   Eval: Human 2 (Human_Evaluation_Results_differences.csv)

# Handle NA
# Attention: Handled differently than MachineEval dataset!
#   OLD PROMPT
heval2[heval2 == "N/A"] <- "NONE" # replace all "N/A" > "NONE"

# Cols

# harmonize col names
heval2_names <- c(
    "source",
    "evaluator",
    "id", # only in 2nd dataset "heval2"
    "dataset",
    "datapoint",
    "title", # only in meval & heval2, not used in heval1
    "smm_consistency",
    "atc_coalition",
    "atc_spd",
    "atc_fdp",
    "atc_green",
    "atc_scholz",
    "atc_lindner",
    "atc_habeck",
    "smm_coalition",
    "smm_spd",
    "smm_fdp",
    "smm_green",
    "smm_scholz",
    "smm_lindner",
    "smm_habeck",
    "comment"
)

# add identifiers ## Must come *before* colnames assignment!
heval2 <- heval2 %>%
    mutate(source = "heval2") %>% # add source file
    select(source, everything()) # move to front

# colnames to df
colnames(heval2) <- heval2_names

# Drop cols
heval2 <- heval2 %>%
    select(-id, -title) # drop id, only in "heval2"

# Change data
heval2 <- heval2 %>% # fix evaluator naming
    mutate(evaluator = if_else(evaluator == "Charly", "Charlotte", evaluator))

# Rows
heval2 <- heval2 %>%
    slice(-(1:2)) # drop 1:2

heval2 <- heval2 %>% # Drop non-eligible: consistency=NONE *OR* datapoint=NONE
    filter(smm_consistency != "NONE" & datapoint != "NONE")

# Clean
rm(heval2_names)


### MANIPULATE DATA

# Combine datasets
heval_long <- bind_rows(heval1, heval2) # df must have same columns

# Combine cols (dataset & datapoint)
heval_long <- heval_long %>%
    mutate(datapoint_new = paste(dataset, datapoint, sep = "_"))
# mutate(datapoint_new = str_c(dataset, datapoint, sep = "_", na.rm = TRUE))
# select(-dataset, -datapoint)


# Unique Identifiers
#   Create hash (rather than uuid > make individual case discoverable)
#   > xxhashlite
#   > new hash col

# Clean
rm(heval1, heval2)



### IDENTIFY DUPLICATES

# Keep first occurrence of duplicates in original df
heval <- heval_long %>%
    filter(!duplicated(datapoint_new))
# Copy subsequent duplicates to new df
heval_duplicates <- heval_long %>%
    filter(duplicated(datapoint_new))
# Substract all duplicates from original df
heval_short <- heval %>%
    anti_join(heval_duplicates, by = "datapoint_new") # Only use for allocation, nothing else!

# Col names & Labels
heval_names <- colnames(heval_long)
heval_labels <- c(
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

# Clean
# rm(heval_long, heval_duplicates)



### ALLOCATE
#   > preprocess_allocate_3rd.R


### MASTER datasets
#   Combine heval & meval
master <- bind_rows(heval, meval)

write.xlsx(master,
    file = "data/processed/MASTER_combined.xlsx",
    sheetName = "master",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.csv(master,
    file = "data/processed/MASTER_combined.csv",
    row.names = FALSE,
    quote = TRUE, # Quote strings, factors, and character vectors
    na = "", # How to represent NA values
    fileEncoding = "UTF-8"
)

write.xlsx(heval,
    file = "data/processed/MASTER_human-eval.xlsx",
    sheetName = "heval",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.csv(heval,
    file = "data/processed/MASTER_human-eval.csv",
    row.names = FALSE,
    quote = TRUE, # Quote strings, factors, and character vectors
    na = "", # How to represent NA values
    fileEncoding = "UTF-8"
)
write.xlsx(meval,
    file = "data/processed/MASTER_machine-eval.xlsx",
    sheetName = "meval",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.csv(meval,
    file = "data/processed/MASTER_machine-eval.csv",
    row.names = FALSE,
    quote = TRUE, # Quote strings, factors, and character vectors
    na = "", # How to represent NA values
    fileEncoding = "UTF-8"
)
