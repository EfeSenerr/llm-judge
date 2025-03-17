### Environment
# getwd()
# setwd("")
# Sys.getenv("VAR1")
# rm(list = ls())  # clean global env
# install.packages("")

rm(list = ls())

library(tidyverse)
library(openxlsx)
library(irr)

### IMPORT

df <- read.csv(
    "data/processed/4th/MASTER_combined_4th_DIFFS.csv",
    header = TRUE,
    sep = ",",
    na.strings = c("", "NA"),
    fill = TRUE, # fill empty cells with NA
    stringsAsFactors = FALSE
)
# heval <- read.csv(
#     "data/processed/4th/MASTER_human-eval_4th_DIFFS.csv",
#     header = TRUE, # without headers
#     sep = ",",
#     na.strings = c("", "NA"),
#     fill = TRUE, # fill empty cells with NA
#     stringsAsFactors = FALSE
# )
# meval <- read.csv(
#     "data/processed/MASTER_machine-eval_DIFFS.csv",
#     header = TRUE, # without headers
#     sep = ",",
#     na.strings = c("", "NA"),
#     fill = TRUE, # fill empty cells with NA
#     stringsAsFactors = FALSE
# )

df_long <- df
# df_names <- colnames(df)
df_labels <- c(
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
    "Datapoint_new",
    "Diff: Coalition Breakdown",
    "Diff: SPD",
    "Diff: FDP",
    "Diff: Greens",
    "Diff: Scholz",
    "Diff: Lindner",
    "Diff: Habeck"
)

### PREP

# Set levels, data types

# Make Sentiment pairs (atc_*, smm_*)


### ANALYZE

# calc Cohen's Kappa

# calc Spearman's Coef

# calc Krippendorf's Alpha

# Krippendorff's Alpha
#
# select: evaluator, atc_spd, datapoint_new

# Intercoder reliability (ICR)

### TRANSFORM to LONG FORMAT

df <- df_long

df <- df %>%
    filter(source %in% c("heval3", "meval")) %>% # select eligible
    select(evaluator, matches("^(atc_|smm_)"), datapoint_new)

df_names_short <- colnames(df)[str_detect(colnames(df), "^(atc_|smm_)")]

df <- df %>%
    group_by(datapoint_new) %>% # case identifier
    filter(n() == 2) %>% # only cases with 2 coders
    arrange(desc(datapoint_new)) %>% # sort
    ungroup()


### TRANSFORM to WIDE FORMAT

variables <- df_names_short # Variables to analyze

results <- list() # Empty list to store results
alpha_values <- numeric(length(variables))
names(alpha_values) <- variables

# Loop through each variable
for (i in 1:length(variables)) {
    var <- variables[i]

    # for (var in variables) {
    # Create matrix for the current variable
    var_matrix <- df %>%
        select(datapoint_new, evaluator, all_of(var)) %>%
        pivot_wider(
            id_cols = datapoint_new,
            names_from = evaluator,
            values_from = all_of(var)
        ) %>%
        select(-datapoint_new) %>%
        mutate(across(everything(), as.character))

    # Transpose the matrix
    var_matrix <- var_matrix %>%
        t() %>%
        as.matrix()

    # Calculate Krippendorff's Alpha
    alpha_result <- kripp.alpha(var_matrix, method = "ordinal")

    # Store the result
    results[[var]] <- alpha_result
    alpha_values[i] <- alpha_result$value

    # Print the result
    cat("\nResults for", var, ":\n")
    print(alpha_result)
}

print(alpha_values)

# Convert the vector to a df
alpha_table <- data.frame(
    variable = names(alpha_values),
    value = as.numeric(alpha_values),
    stringsAsFactors = FALSE
)

# Understand results
#   Missing Data (NA):
#   > If an item (datapoint) has missing ratings from
#   all evaluators or only one rating, it might
#   be excluded from the calculation.
#   > Krippendorff requires at least two valid
#   ratings to assess agreement for a datapoint.

# IRC results "atc_scholz":
#   Subjects: 15
#   Raters: 8
#   alpha: 0.648

results[["atc_scholz"]] # Access results

write.csv(alpha_table, "output/alpha_results_krippLLM.csv", row.names = FALSE)

write.xlsx(alpha_table,
    file = "output/alpha_results_krippLLM.xlsx",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)

### Non-loop implementation
#
# atc_scholz_matrix <- df %>%
#     select(datapoint_new, evaluator, atc_scholz) %>%
#     pivot_wider(
#         id_cols = datapoint_new,
#         names_from = evaluator,
#         values_from = atc_scholz
#     ) %>%
#     select(-datapoint_new) %>%
#     mutate(across(everything(), as.character))
#     # as.matrix()
#
# atc_scholz_matrix <- atc_scholz_matrix %>%
#     t() %>%  # transpose
#     as.matrix()
#
# atc_scholz_alpha <- kripp.alpha(atc_scholz_matrix, method = "ordinal")
#
# print(atc_scholz_alpha)
