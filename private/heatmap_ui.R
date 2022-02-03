tabItem(tabName = "heatmap_tab",
	box(
		title = "Heatmap parameters",
		status = "primary",
		solidHeader = TRUE,
		width = 12,
		fluidRow(
			# Color palette
			column(3,
				selectInput(
					inputId = "color",
					label = "Color palette :",
					choices = c(RedBlue="RdYlBu","YellowBlue","Blues"),
					selected = "RedBlue"
				)
			),
			column(6),
			column(3,
				br(),
		    	actionButton(
		    		inputId = "build_heatmap",
		    		label = "Visualize",
		    		class = "btn btn-primary",
		    		width = "100%"
				)
			)
		)
    ),
	uiOutput("heatmap_visualize")
)