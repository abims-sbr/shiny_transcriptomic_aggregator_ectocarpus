tabItem(tabName = "boxplot_tab",
	box(
		title = "Boxplot parameters",
		status = "primary",
		solidHeader = TRUE,
		width = 12,
		fluidRow(
			column(3,
				selectInput(
					inputId = "metadata",
					label = "Metadata",
					choices = c("Samples", "Genes")
				)
			),
			column(6,
				conditionalPanel(
					condition = "input.metadata == 'Samples'",
					column(6,
		    			uiOutput("metadata_sample_x")
		    		),
	    			column(6,
		    			uiOutput("metadata_sample_col")
		    		)
	    		),
	    		conditionalPanel(
					condition = "input.metadata == 'Genes'",
					column(6,
		    			uiOutput("metadata_gene_x")
		    		),
	    			column(6,
		    			uiOutput("metadata_gene_col")
		    		)
	    		)
	    	),
    		column(3,
    			fluidRow(
    				HTML("<b>Mean by replicats :</b>"),
    			),
    			br(),
		    	materialSwitch(
	    			inputId = "boxplot_replicats",,
					value = FALSE,
					status = "success",
					width = "100px"
				)
	    	)
	    ),
    	fluidRow(
    		column(9),
    		column(3,
		    	actionButton(
		    		inputId = "build_boxplot",
		    		label = "Visualize",
		    		class = "btn btn-primary",
		    		width = "100%"
    			)
    		)
		)
    ),
    uiOutput("boxplot_visualize")
)