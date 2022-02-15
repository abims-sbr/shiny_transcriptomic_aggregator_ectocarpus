tabItem(tabName = "home_tab",
	fluidRow(
		box(
			title="User guide",
			status = "primary",
			solidHeader = TRUE,
			width = 12,
			column(12,
				includeMarkdown("home.Rmd")
			)
		)
	)
)