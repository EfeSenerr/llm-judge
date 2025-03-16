# Libraries ####
library(irr)
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)

# krippendorfer alpha

# "data/processed/4th/XXX"
df_heval3 <- read.csv("_OneDrive/_SyncStudium/WS24-25/Aligning GenAI/GenAI_Analysis/MASTER_human-eval_3rd.csv")
df_heval4 <- read.csv("_OneDrive/_SyncStudium/WS24-25/Aligning GenAI/GenAI_Analysis/MASTER_human-eval_4th.csv")

matched_df1 <- semi_join(df_heval3, df_heval4, by = "datapoint_new")
matched_df2 <- semi_join(df_heval4, df_heval3, by = "datapoint_new")
combined_df <- bind_rows(matched_df1, matched_df2)
glimpse(combined_df)

df <- df_heval4 %>%
  select(-comment, -smm_consistency, -datapoint, -dataset, -source)




# einzel test ####
df_einzeltest <- df[order(df$datapoint_new), ]
df_einzeltest <- df_einzeltest[1:4, ] # in this case the top 4 have the same datapoint_view
ratings <- as.matrix(df_einzeltest)
alpha_result <- kripp.alpha(ratings, method = "ordinal")
print(alpha_result)
# funktioniert so - gut



# tidyverse loop ####
alpha_results <- df %>% 
  group_by(datapoint_new) %>% 
  nest() %>% 
  mutate(alpha = map_dbl(data, function(x) {
    if(nrow(x) >= 2) {
      # alle Spalten außer "datapoint_new"
      ratings <- as.matrix(x[ , setdiff(names(x), "datapoint_new")])
      kripp.alpha(ratings, method = "ordinal")$value
    } else {
      NA_real_
    }
  })) %>% 
  select(datapoint_new, alpha)

print(alpha_results)
mean_alpha <- mean(alpha_results$alpha, na.rm = TRUE)
print(mean_alpha)
# funktioniert pro article
write.csv(alpha_results, "intercoder_agg_krippAlpha.csv", row.names = FALSE)



##################

eval_columns <- c("atc_coalition", "atc_spd", "atc_fdp", "atc_green", 
                  "atc_scholz", "atc_lindner", "atc_habeck",
                  "smm_coalition", "smm_spd", "smm_fdp", "smm_green", 
                  "smm_scholz", "smm_lindner", "smm_habeck")

# Berechne für jedes Bewertungskriterium (Spalte) das Krippendorff's Alpha über alle Gruppen
alpha_results <- map_dfr(eval_columns, function(col) {
  # Für jede Gruppe (definiert durch datapoint_new) werden die Bewertungen (Zeilen) gesammelt
  # und eine Rater-ID vergeben
  df_wide <- df %>% 
    group_by(datapoint_new) %>%
    mutate(rater = row_number()) %>%
    ungroup() %>%
    # Wähle nur die Spalte "datapoint_new", die aktuelle Bewertungsspalte und die rater-ID
    select(datapoint_new, rater, value = all_of(col)) %>%
    # Forme die Daten um, sodass jede Zeile einem datapoint_new entspricht
    pivot_wider(names_from = rater, values_from = value)
  
  # Entferne die Spalte 'datapoint_new', sodass nur noch die Bewertungsmatrix übrig bleibt:
  ratings_mat <- as.matrix(df_wide %>% select(-datapoint_new))
  
  # Falls in der Matrix weniger als 2 Zeilen (Subjekte) vorliegen, kann Alpha nicht berechnet werden
  if(nrow(ratings_mat) < 2) {
    alpha_val <- NA_real_
  } else {
    alpha_val <- kripp.alpha(ratings_mat, method = "ordinal")$value
  }
  
  data.frame(Aspect = col, Alpha = alpha_val, stringsAsFactors = FALSE)
})

print(alpha_results)


# Helper function to compute Krippendorff's alpha for a given evaluation column
compute_alpha_for_column <- function(data, colname, method = "nominal") {
  # For each datapoint_new, assign a unique rater id then pivot to wide format
  df_wide <- data %>%
    group_by(datapoint_new) %>%
    mutate(rater = row_number()) %>%
    ungroup() %>%
    select(datapoint_new, rater, rating = .data[[colname]]) %>%
    pivot_wider(names_from = rater, values_from = rating)
  
  # Remove the unit identifier and convert the remaining data to a matrix
  ratings_mat <- as.matrix(df_wide %>% select(-datapoint_new))
  
  # Calculate and return Krippendorff's alpha
  alpha_result <- kripp.alpha(ratings_mat, method = method)
  return(alpha_result$value)
}

# Identify evaluation columns (adjust the pattern if needed)
eval_columns <- grep("^(atc_|smm_)", names(combined_df), value = TRUE)

# Loop through each evaluation column and calculate its Krippendorff's alpha
alpha_results <- sapply(eval_columns, function(col) {
  compute_alpha_for_column(df, col)
})

# Save the results in a data frame with the column name and corresponding alpha
alpha_df <- data.frame(
  Aspect = names(alpha_results),
  Alpha = as.numeric(alpha_results),
  stringsAsFactors = FALSE
)

# View the result
print(alpha_df)
