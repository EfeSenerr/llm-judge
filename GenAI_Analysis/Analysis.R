# evaluation script

# libraries ####
install.packages("irr")
library(irr)

# preproccessing:

# human_evaluation ####
# delete evaluators, uneccassary columns and column titles
#df_human1 <- human_evalutation
df_human1 <- human_evaluation2
df_human1 <- df_human1[,-1]
df_human1 <- df_human1[-1,]
#df_human1[df_human1 == "N/A"] <- NA
df_human1[is.na(df_human1)] <- "N/A"

# not evaluated rows
df_human1 <- df_human1[-(16:20),]
df_human1 <- df_human1[-(38:45),]
df_human1 <- df_human1[-40,]
df_human1 <- df_human1[-(41:54),]

#uniform column names
names(df_human1)[names(df_human1) == "Consistency"] <- "Summary_Consistency"
names(df_human1)[names(df_human1) == "Scholz Sentiment...8"] <- "Article_Olaf_Scholz"
names(df_human1)[names(df_human1) == "SPD Sentiment...5"] <- "Article_SPD"
names(df_human1)[names(df_human1) == "Habeck Sentiment...10"] <- "Article_Robert_Habeck"
names(df_human1)[names(df_human1) == "Gruenen Sentiment...7"] <- "Article_die_Gruenen"
names(df_human1)[names(df_human1) == "Lindner Sentiment...9"] <- "Article_Christian_Lindner"
names(df_human1)[names(df_human1) == "FDP Sentiment...6"] <- "Article_FDP"
names(df_human1)[names(df_human1) == "Coalition Breakdown Sentiment...4"] <- "Article_the_coalition_breakdown"
names(df_human1)[names(df_human1) == "Scholz Sentiment...15"] <- "Summary_Olaf_Scholz"
names(df_human1)[names(df_human1) == "SPD Sentiment...12"] <- "Summary_SPD"
names(df_human1)[names(df_human1) == "Habeck Sentiment...17"] <- "Summary_Robert_Habeck"
names(df_human1)[names(df_human1) == "Gruenen Sentiment...14"] <- "Summary_die_Gruenen"
names(df_human1)[names(df_human1) == "Lindner Sentiment...16"] <- "Summary_Christian_Lindner"
names(df_human1)[names(df_human1) == "FDP Sentiment...13"] <- "Summary_FDP"
names(df_human1)[names(df_human1) == "Coalition Breakdown Sentiment...11"] <- "Summary_the_coalition_breackdown"

#uniform column names human2 
names(df_human1)[names(df_human1) == "Consistency"] <- "Summary_Consistency"
names(df_human1)[names(df_human1) == "...11"] <- "Article_Olaf_Scholz"
names(df_human1)[names(df_human1) == "...8"] <- "Article_SPD"
names(df_human1)[names(df_human1) == "...13"] <- "Article_Robert_Habeck"
names(df_human1)[names(df_human1) == "...10"] <- "Article_die_Gruenen"
names(df_human1)[names(df_human1) == "...12"] <- "Article_Christian_Lindner"
names(df_human1)[names(df_human1) == "...9"] <- "Article_FDP"
names(df_human1)[names(df_human1) == "Article Evaluation Scores"] <- "Article_the_coalition_breakdown"
names(df_human1)[names(df_human1) == "...18"] <- "Summary_Olaf_Scholz"
names(df_human1)[names(df_human1) == "...15"] <- "Summary_SPD"
names(df_human1)[names(df_human1) == "...20"] <- "Summary_Robert_Habeck"
names(df_human1)[names(df_human1) == "...17"] <- "Summary_die_Gruenen"
names(df_human1)[names(df_human1) == "...19"] <- "Summary_Christian_Lindner"
names(df_human1)[names(df_human1) == "...16"] <- "Summary_FDP"
names(df_human1)[names(df_human1) == "Summary Evaluation Scores"] <- "Summary_the_coalition_breackdown"

##### batch names:
# Correct column names for df_human1
# names(df_human1)[names(df_human1) == "Article File Name"] <- "batch_name"
df_human1$`batch_name`[df_human1$`batch_name` == "newspaper_ampel-aus_0-200_processed.json"] <- "ampel_aus_0"
df_human1$`batch_name`[df_human1$`batch_name` == "newspaper_D-Day_processed.json"] <- "DDay"
df_human1$`batch_name`[df_human1$`batch_name` == "articles_newswires_processed.json"] <- "articles_newswires"
df_human1$`batch_name`[df_human1$`batch_name` == "articles_newspapers_processed.json"] <- "articles_newspapers"
df_human1$`batch_name`[df_human1$`batch_name` == "newspaper_ampel-aus_201-300_processed.json"] <- "ampel_aus_201"
df_human1$`batch_name`[df_human1$`batch_name` == "newspaper_ampel-aus_300-400_processed.json"] <- "ampel_aus_300"
df_human1$`batch_name`[df_human1$`batch_name` == "newspaper_Ampel_Koalition am Ende_52_processed.json"] <- "ampel_koalition"
df_human1$`batch_name`[df_human1$`batch_name` == "newspaper_koalitionskrise_52_processed.json"] <- "koalitionskrise"

names(df_human1)[names(df_human1) == "article_number (in the batch_name file)"] <- "article_number"

# df_human1 <- df_human1[,-4]
# df_human1 <- df_human1[,-1]
# df_human1 <- df_human1[,-18]

df_human1_all_clean <- df_human1
df_human2_all_clean <- df_human1

df_human1_all_clean <- df_human1_all_clean[-(42:112),]

# human unification:
df_combined_human <- rbind(df_human1_all_clean, df_human2_all_clean)

#unique id:
df_combined_human$unique_id <- paste(df_combined_human$article_number, df_combined_human$batch_name, sep = "_")
df_combined_human <- df_combined_human[,-(1:2)]

# delete useless info:
# df_human1 <- df_human1[, !names(df_human1) %in% c("batch_name", "article_number")]

#----aggregate table
#value_counts <- table(df_human1$Summary_die_Gruenen)
#value_counts <- table(df_llm$Summary_Consistency)
print(value_counts)

# To extract the count for "1":
total_ones <- value_counts["1"]
cat("Total ones:", total_ones, "\n")



# llm_evaluation ####
llm_evalution_2 <- llm_evaluation_updated_prompt[,-1]
df_llm <- llm_evalution_2
#df_llm[df_llm == "N/A"] <- NA
df_llm[is.na(df_llm)] <- "N/A"

#correct column names
names(df_llm)[names(df_llm) == "Article File Name"] <- "batch_name"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_ampel-aus_0-200_processed.json"] <- "ampel_aus_0"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_D-Day_processed.json"] <- "DDay"
df_llm$`batch_name`[df_llm$`batch_name` == "articles_newswires_processed.json"] <- "articles_newswires"
df_llm$`batch_name`[df_llm$`batch_name` == "articles_newspapers_processed.json"] <- "articles_newspapers"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_ampel-aus_201-300_processed.json"] <- "ampel_aus_201"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_ampel-aus_300-400_processed.json"] <- "ampel_aus_300"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_Ampel_Koalition am Ende_52_processed.json"] <- "ampel_koalition"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_koalitionskrise_52_processed.json"] <- "koalitionskrise"
#df_llm <- df_llm[, -c(20:27)]
df_llm <- df_llm[-1,]
df_llm <- df_llm[-175,]

#unique id:
names(df_llm)[names(df_llm) == "Article_Number"] <- "article_number"
df_llm$unique_id <- paste(df_llm$article_number, df_llm$batch_name, sep = "_")

#delete useless info:
df_llm <- df_llm[, !names(df_llm) %in% c("ID", "batch_name", "article_number", "Article_Name")]

# aggregate table
for (colname in names(df_combined_human)) {
  cat("Counts for column:", colname, "\n")
  print(table(df_combined_human[[colname]], useNA = "ifany"))
  cat("\n")
}




# Analysis #####

df_analysis <- merge(df_combined_human, df_llm, by = "unique_id", suffixes = c("_human", "_llm"))
df_analysis[is.na(df_analysis)] <- "N/A"

# Cohen's Kappa

# Convert binary columns to factors
df_analysis$Summary_Consistency_human <- as.factor(df_analysis$Summary_Consistency_human)
df_analysis$Summary_Consistency_llm   <- as.factor(df_analysis$Summary_Consistency_llm)

# Compute Cohen's Kappa for binary accuracy
kappa_accuracy <- kappa2(df_analysis[, c("Summary_Consistency_human", "Summary_Consistency_llm")])
print(kappa_accuracy)

# List pairs of full column names
sentiment_pairs <- list(
  c("Article_SPD_human", "Article_SPD_llm"),
  c("Article_FDP_human", "Article_FDP_llm"),
  c("Article_die_Gruenen_human", "Article_die_Gruenen_llm"),
  c("Article_Olaf_Scholz_human", "Article_Olaf_Scholz_llm"),
  c("Article_Christian_Lindner_human", "Article_Christian_Lindner_llm"),
  c("Article_Robert_Habeck_human", "Article_Robert_Habeck_llm")
)

# With "N/A" as a category ####
df_merged <- df_analysis
# For Summary_Consistency, treat missing and explicit "N/A" as the category "N/A"
df_merged$Summary_Consistency_human <- factor(
  ifelse(is.na(df_merged$Summary_Consistency_human) | 
           df_merged$Summary_Consistency_human == "N/A", 
         "N/A", 
         as.character(df_merged$Summary_Consistency_human)),
  levels = c("0", "1", "N/A")
)

df_merged$Summary_Consistency_llm <- factor(
  ifelse(is.na(df_merged$Summary_Consistency_llm) | 
           df_merged$Summary_Consistency_llm == "N/A", 
         "N/A", 
         as.character(df_merged$Summary_Consistency_llm)),
  levels = c("0", "1", "N/A")
)

# Now compute Cohen's kappa including "N/A" as a category
kappa_accuracy_with_NA <- kappa2(df_merged[, c("Summary_Consistency_human", "Summary_Consistency_llm")], weight = "unweighted")
print(kappa_accuracy_with_NA)



# Loop through each pair and compute weighted kappa ####
for (pair in sentiment_pairs) {
  # Convert to ordered factors
  df_analysis[[pair[1]]] <- factor(df_analysis[[pair[1]]], levels = c(-1, 0, 1), ordered = TRUE)
  df_merged[[pair[2]]] <- factor(df_analysis[[pair[2]]], levels = c(-1, 0, 1), ordered = TRUE)
  df_analysis
  kappa_val <- kappa2(df_analysis[, pair], weight = "equal")
  cat("Weighted Cohen's kappa for", pair[1], "and", pair[2], ":", kappa_val$value, "\n")
}

# Recode NA as a level (e.g., "Missing")
df_NA_extra <- df_analysis
df_NA_extra$Summary_Consistency_human <- factor(ifelse(is.na(df_NA_extra$Summary_Consistency_human),
                                                       "Missing",
                                                       df_NA_extra$Summary_Consistency_human),
                                                levels = c("0", "1", "Missing"))

df_NA_extra$Summary_Consistency_llm <- factor(ifelse(is.na(df_NA_extra$Summary_Consistency_llm),
                                                     "Missing",
                                                     df_NA_extra$Summary_Consistency_llm),
                                              levels = c("0", "1", "Missing"))

kappa_result <- kappa2(df_NA_extra[, c("Summary_Consistency_human", "Summary_Consistency_llm")])
print(kappa_result)

#### Spearman / Kendall Coeficient #####

article_sentiment_cols <- c("Article_SPD", "Article_FDP", "Article_die_Gruenen", 
                            "Article_Olaf_Scholz", "Article_Christian_Lindner", "Article_Robert_Habeck", "Summary_Consistency")

cat("Pairwise Rank Correlations (Across All Articles):\n")
for (col in article_sentiment_cols) {
  col_human <- paste0(col, "_human")
  col_llm   <- paste0(col, "_llm")
  
  # Make sure the columns are numeric or factors with numeric levels.
  # (Here we assume the columns are already numeric or integer coded as -1, 0, 1.)
  # If not, you might need to convert them:
  df_analysis[[col_human]] <- as.numeric(as.character(df_analysis[[col_human]]))
  df_analysis[[col_llm]]   <- as.numeric(as.character(df_analysis[[col_llm]]))
  
  # Compute Spearman's rho and Kendall's tau using complete cases
  spearman_val <- cor(df_analysis[[col_human]], df_analysis[[col_llm]], 
                      method = "spearman", use = "complete.obs")
  kendall_val  <- cor(df_analysis[[col_human]], df_analysis[[col_llm]], 
                      method = "kendall", use = "complete.obs")
  
  cat("For", col, ":\n")
  cat("  Spearman's rho:", round(spearman_val, 3), "\n")
  cat("  Kendall's tau:  ", round(kendall_val, 3), "\n\n")
}
