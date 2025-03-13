### Environment
# getwd()
# setwd("")
# Sys.getenv("VAR1")
# rm(list = ls())  # clean global env
# install.packages("")

### GHC prompt:
# Let's code in R today. My dataset is called df. I have imported tidyverse and want to use it whenever possible. Do not repeat the content of my file unless I ask you to.

library(tidyverse)
library(ggplot2)
library(lubridate)
library(svglite)

### IMPORT

heval1 <- read.csv(
    "data/raw/Human_Evaluation_Results.csv",
    header = FALSE, # without headers
    sep = ",",
    na.strings =c("", "NA"),
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
    "data/raw/evaluation_results_aggregated_0503.csv",
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
#meval[is.na(meval)] <- NA

# Cols
meval <- meval %>%
    # harmonize col names
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

meval <- meval %>%
    # drop cols
    select(-title) # not used in heval1,2
    select(-c(
        "Comparison_Olaf_Scholz",
        "Comparison_SPD",
        "Comparison_Robert_Habeck",
        "Comparison_die_Gruenen",
        "Comparison_Christian_Lindner",
        "Comparison_FDP",
        "Comparison_the_coalition_breakdown"
    ))

meval <- meval %>%
    # add evaluator name = LLM
    mutate(
        evaluator = "Llama" # Llama-3.1-8B-Instruct
    ) %>%
    # move new col to front
    select(evaluator, everything())

# Rows
meval <- meval %>%
    #slice(-1) # drop 1st empty
    # Drop non-eligible: consistency=NONE *OR* datapoint=NONE
    filter(smm_consistency != "NONE" & datapoint != "NONE")



### CLEANING
#   Eval: Human 1 (Human_Evaluation_Results.csv)

# Handle NA
# Attention: Handled differently than MachineEval dataset!
#   OLD PROMPT
heval1[heval1 == "N/A"] <- "NONE" # replace all "N/A" > "NONE"

# Cols
# harmonize col names
heval1_names <- c(
    "evaluator",
    #"id", # only in 2nd dataset "heval2"
    "dataset",
    "datapoint",
    #"title", only in meval & heval2, not used in heval1
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

colnames(heval1) <- heval1_names

# Drop cols

# Change data
heval1 <- heval1 %>%
    mutate(dataset = dataset %>%
        str_replace("ampel_aus_0", "newspaper_ampel-aus_0-200_processed.json") %>%
        str_replace("DDay", "newspaper_D-Day_processed.json") %>%
        str_replace("articles_newswires", "articles_newswires_processed.json") %>%
        str_replace("articles_newspapers", "articles_newspapers_processed.json") %>%
        str_replace("ampel_aus_201", "newspaper_ampel-aus_201-300_processed.json") %>%
        str_replace("ampel_aus_300", "newspaper_ampel-aus_300-400_processed.json") %>%
        str_replace("ampel_koalition", "newspaper_Ampel_Koalition am Ende_52_processed.json") %>%
        str_replace("koalitionskrise", "newspaper_koalitionskrise_52_processed.json"))

# Rows
heval1 <- heval1 %>%
    slice(-(1:2)) # drop 1:2
        
heval1 <- heval1 %>%
    # Drop non-eligible: consistency=NONE *OR* datapoint=NONE
    filter(smm_consistency != "NONE" & datapoint != "NONE")


### CLEANING
#   Eval: Human 2 (Human_Evaluation_Results_differences.csv)

# Handle NA
# Attention: Handled differently than MachineEval dataset!
#   OLD PROMPT
heval2[heval2 == "N/A"] <- "NONE" # replace all "N/A" > "NONE"

# Cols
# harmonize col names
heval2_names <- c(
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

colnames(heval2) <- heval2_names

# Drop cols
heval2 <- heval2 %>%
    select(-id, -title) # drop id, only in "heval2"

# Rows
heval2 <- heval2 %>%
    slice(-(1:2)) # drop 1:2

heval2 <- heval2 %>%
    # Drop non-eligible: consistency=NONE *OR* datapoint=NONE
    filter(smm_consistency != "NONE" & datapoint != "NONE")


### MANIPULATE DATA

# Combine datasets
heval <- bind_rows(heval1, heval2) # df must have same columns

# Combine cols (dataset & datapoint)
heval <- heval %>%
    mutate(datapoint_new = str_c(dataset, datapoint, sep = "_", na.rm = TRUE))
    # select(-dataset, -datapoint)

    # Unique Identifiers
    #   Create hash (rather than uuid > make individual case discoverable)
    #   > xxhashlite
    #   > new hash col

### ALLOCATE
evaluator_names <- c("Ayten", "Hannah", "Monika", "Charlotte", "Jannis", "Bene", "Henri", "Efe", "Sophia", "Tim")

for (name in evaluator_names) {
    assign(name, heval %>%
        filter(evaluator == name) %>%
        pull(datapoint_new))
}


evaluator_vectors <- list() # create empty list

# Loop through each name and create a vector
for (name in evaluator_names) {
    evaluator_vectors[[name]] <- heval %>%
        filter(evaluator == name) %>%
        pull(datapoint_new)
}




Henri <- heval %>%
    filter(evaluator == "Henri") %>%
    pull(datapoint_new) # extracts a column as a vector

-create vector named "person" = indiv uids
-create "heval2uids" vector of all uids

-subtract "person" from "heval2uids"
(-count objects in vector)
-choose x values randomly

available:Ayten,Monika**,Hannah,Sophia,Jannis,Charlotte,Bene,(Tim)

### SAVE DATASET

# to memory
df_long <- df  # %>% slice(1:10)
# to CSV
write.csv(df, "df.csv", sep = ",", row.names = FALSE, col.names = TRUE)


# DIFFS

see file