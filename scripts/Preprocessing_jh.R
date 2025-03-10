# [1] Pre-proccessing

# libraries ####
install.packages("irr")
library(irr)

# preproccessing:

# human_evaluation ####
# delete evaluators, uneccassary columns and column titles
# df_human1 <- human_evalutation
df_human1 <- human_evaluation2 # load data file
df_human1 <- df_human1[, -1] # drop "Evaluator Name"
df_human1 <- df_human1[-1, ] # drop empty top row
# df_human1[df_human1 == "N/A"] <- NA
df_human1[is.na(df_human1)] <- "N/A"

# not evaluated rows - carefull... check specific rows!!
# df_human1 <- df_human1[-(16:20),]
# df_human1 <- df_human1[-(38:45),]
# df_human1 <- df_human1[-40,]
# df_human1 <- df_human1[-(41:54),]

# uniform column names
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

# uniform column names human2
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

df_human1_all_clean <- df_human1_all_clean[-(42:112), ]

# human unification:
df_combined_human <- rbind(df_human1_all_clean, df_human2_all_clean)

# unique id:
df_combined_human$unique_id <- paste(df_combined_human$article_number, df_combined_human$batch_name, sep = "_")
df_combined_human <- df_combined_human[, -(1:2)]

# delete useless info:
# df_human1 <- df_human1[, !names(df_human1) %in% c("batch_name", "article_number")]

#----aggregate table
# value_counts <- table(df_human1$Summary_die_Gruenen)
# value_counts <- table(df_llm$Summary_Consistency)
print(value_counts)

# To extract the count for "1":
total_ones <- value_counts["1"]
cat("Total ones:", total_ones, "\n")



# llm_evaluation ####
llm_evalution_2 <- llm_evaluation_updated_prompt[, -1]
df_llm <- llm_evalution_2
# df_llm[df_llm == "N/A"] <- NA
df_llm[is.na(df_llm)] <- "N/A"

# correct column names
names(df_llm)[names(df_llm) == "Article File Name"] <- "batch_name"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_ampel-aus_0-200_processed.json"] <- "ampel_aus_0"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_D-Day_processed.json"] <- "DDay"
df_llm$`batch_name`[df_llm$`batch_name` == "articles_newswires_processed.json"] <- "articles_newswires"
df_llm$`batch_name`[df_llm$`batch_name` == "articles_newspapers_processed.json"] <- "articles_newspapers"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_ampel-aus_201-300_processed.json"] <- "ampel_aus_201"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_ampel-aus_300-400_processed.json"] <- "ampel_aus_300"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_Ampel_Koalition am Ende_52_processed.json"] <- "ampel_koalition"
df_llm$`batch_name`[df_llm$`batch_name` == "newspaper_koalitionskrise_52_processed.json"] <- "koalitionskrise"
# df_llm <- df_llm[, -c(20:27)]
df_llm <- df_llm[-1, ]
df_llm <- df_llm[-175, ]

# unique id:
names(df_llm)[names(df_llm) == "Article_Number"] <- "article_number"
df_llm$unique_id <- paste(df_llm$article_number, df_llm$batch_name, sep = "_")

# delete useless info:
df_llm <- df_llm[, !names(df_llm) %in% c("ID", "batch_name", "article_number", "Article_Name")]

# aggregate table
for (colname in names(df_combined_human)) {
  cat("Counts for column:", colname, "\n")
  print(table(df_combined_human[[colname]], useNA = "ifany"))
  cat("\n")
}
