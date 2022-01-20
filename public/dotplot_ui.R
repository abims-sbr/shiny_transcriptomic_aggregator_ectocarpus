tabItem(tabName = "dotplot_tab",
	box(
		title = "Dotplot parameters",
		status = "primary",
		solidHeader = TRUE,
		width = 12,
		fluidRow(
			column(3,
				uiOutput("dotplot_x_sample")
			),
			column(3,
				uiOutput("dotplot_y_sample")
			),
			column(3),
    		column(3,
    			br(),
		    	actionButton(
		    		inputId = "build_dotplot",
		    		label = "Visualize",
		    		class = "btn btn-primary",
		    		width = "100%"
    			)
    		)
		)
	),
	uiOutput("dotplot_visualize")
)