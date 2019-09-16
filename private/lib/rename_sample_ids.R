# Fonction pour renommer les sample_id avec le contenu de chaque colonne
rename_samples_id <- function(samples_data_table, tpms_data_table){
	cols <- colnames(samples_data_table)
	# Création d'une colonne nom complet
	samples_data_table$sname <- do.call(paste, c(samples_data_table[cols], sep=":"))
	# Déplace la nouvelle colonne en première colonne
	samples_data_table <- samples_data_table %>% select(sname, everything())

	# Renommage des samples selon les nouveaux noms dans tpms
	names(tpms_data_table) <- samples_data_table$sname[match(names(tpms_data_table), samples_data_table$sample_id)]

	# Suppression de la colonne sample_id et renommage de la nouvelle colonne
	samples_data_table$sample_id <- NULL
	names(samples_data_table)[names(samples_data_table)=="sname"]<-"sample_id"

	return(list("samples_data_table" = samples_data_table, "tpms_data_table" = tpms_data_table))
}