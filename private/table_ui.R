tabItem(tabName = "table_tab",
	fluidRow(
		column(5,
			uiOutput("samples_filters")
		),
		column(5,
			uiOutput("genes_filters")
		),
		column(2,
			actionButton(
				inputId = "apply_filters",
				label = "Apply filters",
				class = "btn btn-primary",
				width = "100%"
			),
			actionButton(
				inputId = "reset_all_filters",
				label = "Reset all filters",
				width = "100%"
			)
		)		
	),
	# DataTable
	fluidRow(
		column(2,
			uiOutput("samples_id_filter")
		),
		column(10,
			DTOutput('table')
		)
	)
)