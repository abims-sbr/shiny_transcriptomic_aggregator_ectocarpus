### Shiny app configurations ###

# Name of the project
project <- "Ectocarpus"

# Instance status ("public" OR "private")
instance_tag <- "public"

# Columns to ignore in metadata file as filter
gene_col_blacklist <- c("description", "content")
sample_col_blacklist <- c("replicats")

# Project Files
tpms_input <- "data/tpms.csv"
genes_data_input <- "data/genes.csv"
samples_data_input <- "data/samples.csv"
