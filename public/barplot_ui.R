tabItem(tabName = "barplot_tab",
	box(
		title = "Barplot parameters",
		status = "primary",
		solidHeader = TRUE,
		width = 12,
		# TODO : Create the filters
    	fluidRow(
    		column(9),
    		column(3,
		    	actionButton(
		    		inputId = "build_barplot",
		    		label = "Visualize",
		    		class = "btn btn-primary",
		    		width = "100%"
    			)
    		)
		)
    ),
    uiOutput("barplot_visualize")
)