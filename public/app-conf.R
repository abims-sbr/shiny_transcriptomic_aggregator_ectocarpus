### Shiny app configurations ###

# Name of the project
project <- "Ectocarpus"

# Instance status ("public" OR "private")
instance_tag <- "public"

# Columns to ignore in genes metadata file
gene_col_blacklist <- c("description", "content")
sample_col_blacklist <- c("replicats")

# Project Files
tpms_input <- "real-data/TPMs.csv"
genes_data_input <- "real-data/Genes_data.csv"
samples_data_input <- "real-data/Samples_data.csv"
