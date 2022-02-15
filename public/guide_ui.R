tabItem(tabName = "guide_tab",
	fluidRow(
		box(
			title="User guide",
			status = "primary",
			solidHeader = TRUE,
			width = 12,
			column(12,
				includeMarkdown("user_guide.Rmd")
			)
		)
	)
)