# Dotplot UI
output$dotplot_tab <- renderUI({
	if(!is.null(tpms_data_table()) && !is.null(genes_data_table()) && !is.null(samples_data_table())){
		tagList(
			box(
				title = "Dotplot",
				status = "primary",
				solidHeader = TRUE,
				width = 12,
				fluidRow(
					column(6,
		    			uiOutput("dotplot_sample_one")
		    		),
		    		column(6,
		    			uiOutput("dotplot_sample_two")
		    		)
	    		),
	    		fluidRow(
	    			column(8,
		    			selectInput(
		        			inputId = "dotplot_ext",
		        			label = "Export format :",
		                	choices = c("PNG", "PDF", "SVG", "EPS"),
		                	width = "200px"
		    			)
	    			),
	    			column(4,
	    				br(),
				    	downloadButton(
				    		outputId = "dotplot",
				    		label = "Dotplot",
				    		width = "100%"
		    			)
			    	)
			    )
		    )
		)
	}
})

# Dotplot selected samples
output$dotplot_sample_one <- renderUI({
	genes_data <- genes_data_table()
	final_table <- final_table()
	tagList(
		selectInput(
			inputId = "dotplot_sample_x",
			label = "X sample :",
			choices = colnames(final_table[,-(1:ncol(genes_data))]),
			width = "180px"
		)
	)
})
output$dotplot_sample_two <- renderUI({
	genes_data <- genes_data_table()
	final_table <- final_table()
	tagList(
		selectInput(
			inputId = "dotplot_sample_y",
			label = "Y sample :",
			choices = subset(colnames(final_table[,-(1:ncol(genes_data))]), !(colnames(final_table[,-(1:ncol(genes_data))]) %in% input$dotplot_sample_x )),
			width = "180px"
		)
	)
})

# Download dotplot
output$dotplot <- downloadHandler (	
	filename = function() {
		paste0("dotplot.", tolower(input$dotplot_ext))
	},
	content = function(file){
		final_table <- final_table()
		buildDotplot(final_table, file, ext = input$dotplot_ext, x=input$dotplot_sample_x, y=input$dotplot_sample_y)
	}
)