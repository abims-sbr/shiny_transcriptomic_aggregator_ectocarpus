# Set default repo from CRAN
options(repos=structure(c(CRAN="https://cran.rstudio.com/")))
# Install some packages
install.packages(c('shiny', 'shinyjs', 'shinyBS', 'DT', 'gplots', 'Hmisc', 'reshape'))

# Load packages
library(shiny)
library(shinyjs)
library(shinyBS)
library(DT)
library(gplots)
library(Hmisc)
library(reshape)
library(data.table)

# Load functions
source("../lib/merge_duplicated_data.R")
source("../lib/read_tables.R")
source("../lib/build_graphs.R")


# User interface
ui <- bootstrapPage(
	includeCSS("../static/css/styles.css"),
	useShinyjs(),
	br(),
	fluidRow(
		# Import Files Panel
		column(3,
			bsCollapse(
				id = "files_panel",
				open = "filters",
				bsCollapsePanel(
					title = h3("Import data"),
					value = "filters",
				 	fileInput(
				   		inputId = "tpms_file", 
				   		label = "Import TPMs File (.csv)",
				        multiple = FALSE,
				        accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
				        width = "100%"
				    ),
				   	fileInput(
				   		inputId = "genes_data_file", 
				   		label = "Import Genes Data File (.csv)",
				        multiple = FALSE,
				        accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
				        width = "100%"
				    ),
				  	fileInput(
				   		inputId = "samples_data_file", 
				   		label = "Import Samples Data File (.csv)",
				        multiple = FALSE,
				        accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
				        width = "100%"
				    ),
				    fileInput(
				   		inputId = "genes_list_file", 
				   		label = "Import Genes List (.txt/.csv) [OPTIONAL]",
				        multiple = FALSE,
				        accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
				        width = "100%"
				    ),
				    actionButton(
				    	inputId = "reset_files",
				    	label = "Reset"
				    )
			    )
		    ),
			uiOutput("graphs_tab")		    
		),
		# Filters Panels
		column(9,
			uiOutput("samples_filters"),
			uiOutput("genes_filters")
		)
	),
	tags$hr(),
	# DataTable
	fluidRow(
		column(12,
			dataTableOutput('table')
		)
	)
)


# Server function
server <- function(input, output, session){

	# Initialisation
    final_table <- reactiveVal(0)


	# Import files
	samples_data_table <- eventReactive(input$samples_data_file, {
        samples_data_table <- getDataFrameFromFile(input$samples_data_file$datapath)
    })

    genes_data_table <- eventReactive(input$genes_data_file, {
        genes_data_file <- getDataFrameFromFile(input$genes_data_file$datapath)
        genes_data_table <- merge_duplicated_data(genes_data_file)
    })

    tpms_data_table <- eventReactive(input$tpms_file, {
        tpms_data_table <- getDataFrameFromFile(input$tpms_file$datapath)
    })


	# Generate UI Samples Filters
	output$samples_filters <- renderUI({
		samples_data_table <- samples_data_table()
		tagList(
			bsCollapsePanel(
				title = h3("Samples Metadata Filters"),
				fluidRow(
			      	lapply(1:ncol(samples_data_table), function(i) {
			      		if (tolower(colnames(samples_data_table)[i]) != "private") {
				      		column(3,
								selectInput(
				        			inputId = colnames(samples_data_table)[i],
				        			label = paste0(capitalize(gsub("_", " ", colnames(samples_data_table)[i])), " :"),
				                	choices = c("All", sort(unique(unlist(strsplit(as.character(samples_data_table[,i]),","))))),
				                	width = "200px",
				                	selected = "All"
				            	)
				      		)
				      	}
			      	})
				),
				fluidRow(
					column(11),
					column(1,
						actionButton(
							inputId = "reset_samples_values",
							label = "Reset" 
						)
					)
				)				
			)
		)
	})

	# Generate UI Genes Filters
	output$genes_filters <- renderUI({
		genes_data_table <- genes_data_table()
		tagList(
			bsCollapsePanel(
				title = h3("Genes Metadata Filters"),
				fluidRow(
					lapply(1:ncol(genes_data_table), function(i) {
						column(3,
							selectInput(
			        			inputId = colnames(genes_data_table)[i],
			        			label = paste0(capitalize(gsub("_", " ", colnames(genes_data_table)[i])), " :"),
			                	choices = c("All", sort(unique(unlist(strsplit(as.character(genes_data_table[,i]), ","))))),
			                	selected = "All",
			                	width = "200px"
							)
						)
					})
				),
				fluidRow(
					column(11),
					column(1,
						actionButton(
							inputId = "reset_genes_values",
							label = "Reset" 
						)
					)
				)
			)
		)
	})

	# Graphs tab
	output$graphs_tab <- renderUI({
		if(!is.null(input$tpms_file) && !is.null(input$genes_data_file) && !is.null(input$samples_data_file)){
			tagList(
				bsCollapsePanel(
			    	title = h3("Graphs"),
			    	tabsetPanel(
			    		type = "tabs",
			    		tabPanel(
			    			title = "Heatmap",
			    			br(),
			    			fluidRow(
			    				column(8,
					    			selectInput(
					        			inputId = "heatmap_ext",
					        			label = "Export format :",
					                	choices = c("PNG", "PDF", "SVG", "EPS"),
					                	width = "200px"
					    			)
			    				),
			    				column(4,
			    					br(),
					    			downloadButton(
							    		outputId = "heatmap",
							    		label = "Heatmap",
							    		width = "100%"
					    			)
			    				)
			    			)
			    		),
			    		tabPanel(
			    			title = "Boxplot",
			    			br(),
			    			selectInput(
      							inputId = "metadata",
      							label = "Metadata",
      							choices = c("Samples", "Genes")
    						),
				    		fluidRow(
				    			conditionalPanel(
    								condition = "input.metadata == 'Samples'",
				    				column(6,
				    					h4("X Metadata :"),
						    			uiOutput("metadata_sample_x")
						    		),
					    			# Y Metadata : log2(TPMs)
					    			column(6,
						    			h4("Color by :"),
						    			uiOutput("metadata_sample_col")
						    		)
					    		),
					    		conditionalPanel(
    								condition = "input.metadata == 'Genes'",
				    				column(6,
				    					h4("X Metadata :"),
						    			uiOutput("metadata_gene_x")
						    		),
					    			# Y Metadata : log2(TPMs)
					    			column(6,
						    			h4("Color by :"),
						    			uiOutput("metadata_gene_col")
						    		)
					    		)
					    	),
					    	hr(),
					    	fluidRow(
					    		column(8,
							    	selectInput(
					        			inputId = "boxplot_ext",
					        			label = "Export format :",
					                	choices = c("PNG", "PDF", "SVG", "EPS"),
					                	width = "200px"
					    			)
					    		),
					    		column(4,
					    			br(),
							    	downloadButton(
							    		outputId = "boxplot",
							    		label = "Boxplot",
							    		width = "100%"
					    			)
					    		)
					    	)
			    		),
			    		tabPanel(
			    			title = "Dotplot",
			    			br(),
			    			fluidRow(
			    				column(6,
			    					h4("X sample :"),
					    			uiOutput("dotplot_sample_one")
					    		),
					    		column(6,
					    			h4("Y sample :"),
					    			uiOutput("dotplot_sample_two")
					    		)
				    		),
				    		hr(),
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
		    	)
			)
		}
	})

	# Boxplot selected metadata
	output$metadata_sample_x <- renderUI({
		samples_data <- samples_data_table()
		genes_data <- genes_data_table()
		tagList(
			selectInput(
				inputId = "meta_sample_x",
				label = NULL,
				choices = c(colnames(samples_data)),
				width = "180px"
			)
		)
	})

	output$metadata_sample_col <- renderUI({
		samples_data <- samples_data_table()
		genes_data <- genes_data_table()
		tagList(
			selectInput(
				inputId = "meta_sample_col",
				label = NULL,
				choices = subset(c(colnames(samples_data)), !(c(colnames(samples_data)) %in% input$meta_sample_x)),
				width = "180px"
			)
		)
	})

	output$metadata_gene_x <- renderUI({
		samples_data <- samples_data_table()
		genes_data <- genes_data_table()
		tagList(
			selectInput(
				inputId = "meta_gene_x",
				label = NULL,
				choices = c(colnames(genes_data)),
				width = "180px"
			)
		)
	})

	output$metadata_gene_col <- renderUI({
		samples_data <- samples_data_table()
		genes_data <- genes_data_table()
		tagList(
			selectInput(
				inputId = "meta_gene_col",
				label = NULL,
				choices = subset(c(colnames(genes_data)), !(c(colnames(genes_data)) %in% input$meta_gene_x)),
				width = "180px"
			)
		)
	})

	# Dotplot selected samples
	output$dotplot_sample_one <- renderUI({
		samples_data <- samples_data_table()
		tagList(
			selectInput(
				inputId = "dotplot_sample1",
				label = NULL,
				choices = unique(samples_data[,1]),
				width = "180px"
			)
		)
	})

	output$dotplot_sample_two <- renderUI({
		samples_data <- samples_data_table()
		tagList(
			selectInput(
				inputId = "dotplot_sample2",
				label = NULL,
				choices = subset(unique(samples_data[,1]), !(unique(samples_data[,1]) %in% input$dotplot_sample1)),
				width = "180px"
			)
		)
	})


	# Events
    genes_list <- reactiveVal(value=NULL)
    observeEvent(input$genes_list_file, {
    	genes_list(c(read.csv(input$genes_list_file$datapath, header=FALSE, sep="\t", stringsAsFactors=FALSE, check.names=FALSE)))
    })

	# Reset filters values
	observeEvent(input$reset_samples_values, {
		samples_data_table <- samples_data_table()
		for ( i in 1:ncol(samples_data_table)) {
        	updateSelectInput(
        		session = session, 
        		inputId = colnames(samples_data_table)[i],
        		selected = "All"
        	)
		}
	})
	observeEvent(input$reset_genes_values, {
		genes_data_table <- genes_data_table()
		for ( i in 1:ncol(genes_data_table)) {
        	updateSelectInput(
        		session = session, 
        		inputId = colnames(genes_data_table)[i],
        		selected = "All"
        	)
		}
  	})
  	observeEvent(input$reset_files, {
  		genes_list(NULL)
  		reset('files_panel')
  	})

	# DataTable
	output$table <- renderDataTable(
		{
        	samples_data <- samples_data_table()
			genes_data_table <- genes_data_table()
			tpms_data <- tpms_data_table()

			# Remove Private column
			if("private" %in% tolower(colnames(samples_data))){
				samples_data["private"] <- NULL
			}

			# Apply filters
			for ( i in 1:ncol(samples_data)) {
        		if (input[[colnames(samples_data)[i]]] != "All") {
				    samples_data <- samples_data[samples_data[i] == input[[colnames(samples_data)[i]]],]
				}
			}

			# Update other filters values
			for ( j in 1:ncol(samples_data)) {
				if (input[[colnames(samples_data)[j]]] == "All") {
				    updateSelectInput(
				       	session = session, 
				       	inputId = colnames(samples_data)[j],
				       	choices = c("All", unique(samples_data[,j])),
				       	selected = "All"
		    		)
				}
	    	}

	    	updateSelectInput(
        		session = session, 
        		inputId = "dotplot_sample1",
        		choices = unique(samples_data[,1]),
        		selected = input$dotplot_sample1
        	)
        	updateSelectInput(
        		session = session, 
        		inputId = "dotplot_sample2",
        		choices = subset(unique(samples_data[,1]), !(unique(samples_data[,1]) %in% input$dotplot_sample1)),
        		selected = input$dotplot_sample2
        	)

	    	# Merge TPMS and genes metadata files according to filters
		    gene_id <- tpms_data[1] # Supposing gene_id is the 1st column
		    filtered_tpms_data <- cbind(gene_id, tpms_data[colnames(tpms_data) %in% samples_data[,1]]) # Supposing sample_id is the 1st column
		    merged_genes_tpms <- merge(genes_data_table, filtered_tpms_data, by=colnames(genes_data_table[1]))
		    final_table <- merged_genes_tpms

		    for ( i in 1:ncol(genes_data_table)){
		    	if (input[[colnames(genes_data_table)[i]]] != "All") {
			    	final_table <- merged_genes_tpms[merged_genes_tpms[,i] %like% input[[colnames(genes_data_table)[i]]],]
				}
			}

	    	# Update table by gene list file
	    	genes_list <- genes_list()
	    	if (length(genes_list) > 0) {
	    		final_table <- subset(final_table, final_table[,1] %in% genes_list[[1]])	
	    	}

		    final_table(final_table)
		    final_table
		},
		rownames = FALSE,
	    extensions = c("Buttons", "FixedHeader"),
        options = list(
        	dom = 'Blfrtip',
        	order = list(0, 'asc'),
        	fixedHeader = TRUE,
        	pageLength = 25,
  			lengthMenu = c(10, 25, 50, 100, 200),
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            scrollX = TRUE
        )
	)

	# Heatmap
	output$heatmap <- downloadHandler (
		filename = function() {
			paste0("heatmap.", tolower(input$heatmap_ext))
		},
		content = function(file) {
			final_table <- final_table()
			tpms_data <- final_table[,-c(2:4)]
			tpms_table <- tpms_data[,-1]
			rownames(tpms_table) <- tpms_data[,1]
			tpms_matrix <- data.matrix(tpms_table)

			buildHeatmap(tpms_matrix, file, ext = input$heatmap_ext)
		}
  	)

  	# Boxplot
	output$boxplot <- downloadHandler (		
		filename = function() {
			paste0("boxplot.", tolower(input$boxplot_ext))
		},
		content = function(file){
			final_table <- final_table()
			genes_data_table <- genes_data_table()
			samples_data_table <- samples_data_table()

			boxplot_data<-final_table[,-c(2:length(genes_data_table))]
			boxplot_choice<-"samples"

			if (input$metadata == "Genes"){
				# Table for genes metadata
				genes_for_boxplot <- boxplot_data[,-c(1)]
				genes_for_boxplot <- cbind(genes_data_table[input$meta_gene_x],genes_data_table[input$meta_gene_col], genes_for_boxplot)
				melted_data<-melt(genes_for_boxplot, id=c(as.character(input$meta_gene_x),as.character(input$meta_gene_col)))
	
				buildBoxplot(melted_data, file, ext = input$boxplot_ext, x = input$meta_gene_x , col = input$meta_gene_col)

			} else if (input$metadata == "Samples") {
				# Transpose table for samples metadata
				transpo_tpms <- data.frame(t(boxplot_data))
				factors <- sapply(transpo_tpms, is.factor)
				transpo_tpms[factors] <- lapply(transpo_tpms[factors], as.character)
				colnames(transpo_tpms) <- transpo_tpms[1,]

				samples_id <- rownames(transpo_tpms[-1,])
				samples_data_table <- samples_data_table[samples_data_table[["sample_id"]] %in% samples_id,]
				
				transpo_tpms <- data.frame(transpo_tpms[-1,], row.names=NULL)
				samples_for_boxplot <- cbind(samples_data_table[input$meta_sample_x],samples_data_table[input$meta_sample_col], transpo_tpms)
				melted_data <- melt(samples_for_boxplot, id=c(as.character(input$meta_sample_x), as.character(input$meta_sample_col)))

				buildBoxplot(melted_data, file, ext = input$boxplot_ext, x = input$meta_sample_x , col = input$meta_sample_col)	
			}
		}
  	)

  	# Dotplot
  	output$dotplot <- downloadHandler (	
		filename = function() {
			paste0("dotplot.", tolower(input$dotplot_ext))
		},
		content = function(file){
			final_table <- final_table()
			buildDotplot(final_table, file, ext = input$dotplot_ext, x=input$dotplot_sample1, y=input$dotplot_sample2)
		}
	)
}

shinyApp(ui, server)