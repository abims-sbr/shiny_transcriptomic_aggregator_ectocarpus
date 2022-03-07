tabItem(tabName = "import_tab",
	box(
		title = "Import data",
		status = "primary",
		solidHeader = TRUE,
		width = 12,
		column(12,
			fluidRow(
			   	fileInput(
			   		inputId = "genes_data_file", 
			   		label = "Import Genes Data File (.csv)",
			        multiple = FALSE,
			        accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
			        width = "100%"
			    )
			),
			fluidRow(
			  	fileInput(
			   		inputId = "samples_data_file", 
			   		label = "Import Samples Data File (.csv)",
			        multiple = FALSE,
			        accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
			        width = "100%"
			    )
		    ),
			fluidRow(
			 	fileInput(
			   		inputId = "tpms_file", 
			   		label = "Import TPMs File (.csv)",
			        multiple = FALSE,
			        accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
			        width = "100%"
			    )
			),
			fluidRow(
				actionButton(
					inputId = "import_files",
					label = "Import",
					class = "btn btn-primary"
				),
			    actionButton(
			    	inputId = "reset_files",
			    	label = "Reset"
			    )
			)
		)
	)
)