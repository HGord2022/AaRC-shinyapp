setwd("~/github_repos/AaRC-shinyapp/AaRC-Metadata")

library(dplyr)
library(googlesheets4)

# read data
gs4_deauth()

sheet_url <- "https://docs.google.com/spreadsheets/d/1me-fjDmVRktAGRvThZuA9O1VX9s_ZYwox2jDbtOhEZI/edit?gid=1182961736#gid=1182961736"

# Define the sheets to use
sheets_to_use <- c("canids", "capra", "ursus", "sus", "felis", "rodent")  # add sheets as they are filled in

# Read and stack only selected sheets
df <- lapply(sheets_to_use, function(s) {
  read_sheet(sheet_url, sheet = s) %>%
    mutate(across(everything(), as.character)) %>%  # convert all columns
    mutate(.sheet = s)  # add column for sheet name
}) %>%
  bind_rows()

df$molecular_sex[!df$molecular_sex %in% c("F", "M")] <- "missing"

# View the combined data
head(df)

write.csv(df, file="aarc_metadata.csv")
