### Shiny app configurations ###

# Name of the project
project <- "Ectocarpus"

# Instance status ("public" OR "private")
instance_tag <- "private"

# Columns to ignore in metadata file
gene_col_blacklist <- c("description", "content")
sample_col_blacklist <- c("replicats")

# Project Files
tpms_input <- "real-data/tpms.csv"
genes_data_input <- "real-data/genes.csv"
samples_data_input <- "real-data/samples.csv"
