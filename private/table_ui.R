tabItem(tabName = "table_tab",
	fluidRow(
		column(6,
			uiOutput("samples_filters")
		),
		column(6,
			uiOutput("genes_filters")
		)
	),
	# DataTable
	fluidRow(
		column(2,
			uiOutput("samples_id_filter")
		),
		column(10,
			dataTableOutput('table')
		)
	)
)