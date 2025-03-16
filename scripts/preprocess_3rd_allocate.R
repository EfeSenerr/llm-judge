# 4th round of evals
# This script allocates articles from [ heval3 ] for validation.


### RUN FIRST
#   preprocess_3rd.R


# Save duplicates from 3rd round as validators in 4th round
#
write.csv(heval3_duplicates,
    file = "_tmp/heval3_duplicates.csv",
    row.names = FALSE,
    quote = TRUE, # Quote strings, factors, and character vectors
    na = "", # How to represent NA values
    fileEncoding = "UTF-8"
)


### ALLOCATE

evaluator_names <- c("Ayten", "Hannah", "Monika", "Charlotte", "Jannis", "Bene", "Henri", "Efe", "Sophia", "Tim")

# Create vector for each name containing respective datapoints
for (name in evaluator_names) {
    assign(name, heval3_short %>%
        filter(evaluator == name) %>%
        pull(datapoint_new))
}
rm(name) # useless artifact

# Subtract name vectors from datapoint_vector
#
#   Tim
#   +1 already done (3rd round)

#   Sophia
datapoint_vector <- heval3_short %>% pull(datapoint_new)
datapoint_vector <- setdiff(datapoint_vector, Sophia)
Sophia_4th <- datapoint_vector %>%
    sample(6)

#  Charlotte
datapoint_vector <- heval3_short %>% pull(datapoint_new)
datapoint_vector <- setdiff(datapoint_vector, Sophia_4th)
datapoint_vector <- setdiff(datapoint_vector, Charlotte)
Charlotte_4th <- datapoint_vector %>%
    sample(3)

#  Bene
datapoint_vector <- heval3_short %>% pull(datapoint_new)
datapoint_vector <- setdiff(datapoint_vector, Sophia_4th)
datapoint_vector <- setdiff(datapoint_vector, Charlotte_4th)
datapoint_vector <- setdiff(datapoint_vector, Bene)
Bene_4th <- datapoint_vector %>%
    sample(3)

#  Jannis
datapoint_vector <- heval3_short %>% pull(datapoint_new)
datapoint_vector <- setdiff(datapoint_vector, Sophia_4th)
datapoint_vector <- setdiff(datapoint_vector, Charlotte_4th)
datapoint_vector <- setdiff(datapoint_vector, Bene_4th)
datapoint_vector <- setdiff(datapoint_vector, Jannis)
Jannis_4th <- datapoint_vector %>%
    sample(3)


# Clean
rm(list = evaluator_names)
# rm(heval_short)


### PREP ALLOCATE OUTPUT
#   filter df for rows where datapoint_new is in name vector

heval4_dupl <- heval3_duplicates %>%
    mutate(source = "heval4")

# Sophia
Sophia_4th_df <- heval3 %>%
    filter(datapoint_new %in% Sophia_4th) %>%
    mutate(source = "heval4") %>% # set source name
    mutate(evaluator = "Sophia") # set evaluator name
Sophia_4th_df <- Sophia_4th_df %>%
    mutate(across(-c(source, evaluator, dataset, datapoint, datapoint_new), ~NA)) # delete all sentiment values

# Charlotte
Charlotte_4th_df <- heval3 %>%
    filter(datapoint_new %in% Charlotte_4th) %>%
    mutate(source = "heval4") %>% # set source name
    mutate(evaluator = "Charlotte") # set evaluator name
Charlotte_4th_df <- Charlotte_4th_df %>%
    mutate(across(-c(source, evaluator, dataset, datapoint, datapoint_new), ~NA)) # delete all sentiment values

# Bene
Bene_4th_df <- heval3 %>%
    filter(datapoint_new %in% Bene_4th) %>%
    mutate(source = "heval4") %>% # set source name
    mutate(evaluator = "Bene") # set evaluator name
Bene_4th_df <- Bene_4th_df %>%
    mutate(across(-c(source, evaluator, dataset, datapoint, datapoint_new), ~NA)) # delete all sentiment values

# Jannis
Jannis_4th_df <- heval3 %>%
    filter(datapoint_new %in% Jannis_4th) %>%
    mutate(source = "heval4") %>% # set source name
    mutate(evaluator = "Jannis") # set evaluator name
Jannis_4th_df <- Jannis_4th_df %>%
    mutate(across(-c(source, evaluator, dataset, datapoint, datapoint_new), ~NA)) # delete all sentiment values


### SAVE DATASET

# Allocated evals
write.xlsx(heval3_duplicates,
    file = "_orga/allocate/heval4_dupl.xlsx",
    sheetName = "heval4_dupl",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.xlsx(Sophia_4th_df,
    file = "_orga/allocate/Sophia4th.xlsx",
    sheetName = "Sophia_4th_df",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.xlsx(Charlotte_4th_df,
    file = "_orga/allocate/Charlotte4th.xlsx",
    sheetName = "Charlotte_4th_df",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.xlsx(Bene_4th_df,
    file = "_orga/allocate/Bene4th.xlsx",
    sheetName = "Bene_4th_df",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
write.xlsx(Jannis_4th_df,
    file = "_orga/allocate/Jannis4th.xlsx",
    sheetName = "Jannis_4th_df",
    asTable = FALSE,
    overwrite = TRUE,
    colNames = TRUE
)
