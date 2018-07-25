#Fonction pour fichier Genes_data
merge_duplicated_data <- function(table){
	before_i_list <- c()
	i_list <- c()
	i_value_list <- c()
	duplicated_data<-duplicated(table$gene_id)

	if ( length(unique(duplicated_data))>1 ) {
		for ( i in 1:length(duplicated_data) ) {
			if ( duplicated_data[i] == TRUE ) {
				before_i_list <- c(before_i_list, i-1)
				i_list <- c(i_list, i)
				i_value_list <- c(i_value_list, as.character(table[i,4]))
			}
		}
		if ( !is.null(before_i_list) ) {
			for ( j in 1:length(before_i_list) ) {
				new_value <- paste0(table$gene_group[as.numeric(before_i_list[j])], "," ,i_value_list[j])
				table[as.numeric(before_i_list[j]),4] = as.character(new_value)
			}
		}
		new_table <- table[-c(as.numeric(i_list)),]
		return(new_table)
	} else {
		return(table)
	}
}