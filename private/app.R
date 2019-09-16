# Set default repo from CRAN
options(repos=structure(c(CRAN="https://cran.rstudio.com/")))
# Update installed packages
#update.packages(ask=FALSE, checkBuilt=TRUE)
# Install some packages
install.packages(c('shiny', 'shinyjs', 'shinyBS', 'DT', 'data.table', 'gplots', 'Hmisc', 'reshape', 'rlist'))


# Load packages
library(shiny)
library(shinyjs)
library(shinyBS)
library(DT)
library(dplyr)
library(gplots)
library(Hmisc)
library(RColorBrewer)
library(rlist)
library(reshape)
library(data.table)

# Load configurations
source("app-conf.R", local = TRUE)

# Load functions
source("lib/merge_duplicated_data.R")
source("lib/read_tables.R")
source("lib/build_graphs.R")


# User interface
ui <- bootstrapPage(
	includeCSS("static/css/styles.css"),
	tags$style(type="text/css",
		".shiny-output-error { visibility: hidden; }",
		".shiny-output-error:before { visibility: hidden; }"
	),
	useShinyjs(),
	fluidRow(
		HTML('<header id="header"> Shiny Transcriptomic Aggregator - '),
        project,
        HTML('</header>')
	),
	br(),
	fluidRow(
		# Import Files Panel
		column(3,
			bsCollapse(
				id = "files_panel",
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
		column(2,
			uiOutput("samples_id_filter")
		),
		column(10,
			dataTableOutput('table')
		)
	),
	fluidRow(
		HTML(
			'<footer id="footer">
            	Copyright (c) 2018-2019 - Station Biologique de Roscoff - ABiMS
            </footer>'
		)
	)
)


# Server function
server <- function(input, output, session){

	## Variable Initialisation	
    final_table <- reactiveVal(0)
    genes_list <- reactiveVal(value=NULL)
    if(!is.null(tpms_input)){
	    tpms_data_table <- reactiveVal(value=getDataFrameFromFile(tpms_input))
    }
    if(!is.null(genes_data_input)){
    	genes_data_file <- getDataFrameFromFile(genes_data_input)
    	genes_data_table <- reactiveVal(value=merge_duplicated_data(genes_data_file))
    }
    if(!is.null(samples_data_input)){
    	samples_data_file <- getDataFrameFromFile(samples_data_input)
		if("private" %in% tolower(colnames(samples_data_file))){
			if(instance_tag == "public"){
				samples_data_file <- samples_data_file[toupper(samples_data_file[,"private"]) == "FALSE",]
			}
			samples_data_file["private"] <- NULL
		}
    	samples_data_table <- reactiveVal(value=samples_data_file)
	}

	## Import files
	# TPMS
    observeEvent(input$tpms_file, {
    	tpms_data_table(getDataFrameFromFile(input$tpms_file$datapath))
    })
	# Genes Data
    observeEvent(input$genes_data_file, {
    	genes_data_file<-getDataFrameFromFile(input$genes_data_file$datapath)
    	genes_data_table(merge_duplicated_data(genes_data_file))
    })
	# Samples Data
    observeEvent(input$samples_data_file, {
    	samples_data_file <- getDataFrameFromFile(input$samples_data_file$datapath)
		if("private" %in% tolower(colnames(samples_data_file))){
			if(instance_tag == "public"){
				samples_data_file <- samples_data_file[toupper(samples_data_file[,"private"]) == "FALSE",]
			}
			samples_data_file["private"] <- NULL
		}
		samples_data_table(samples_data_file)   	
    })
    # Gene List
    observeEvent(input$genes_list_file, {
    	genes_list(c(read.csv(input$genes_list_file$datapath, header=FALSE, sep="\t", stringsAsFactors=FALSE, check.names=FALSE)))
    })


    ### Generate UI Filters
	## Samples Filters
	output$samples_filters <- renderUI({
		samples_data <- samples_data_table()
		tagList(
			bsCollapsePanel(
				title = h3("Samples Metadata Filters"),
				fluidRow(
			      	lapply(1:ncol(samples_data), function(i) {
			      		if (!(tolower(colnames(samples_data)[i]) %in% sample_col_blacklist)) {
			      			if (tolower(colnames(samples_data)[i]) != "sample_id"){
					      		column(3,
									selectInput(
					        			inputId = colnames(samples_data)[i],
					        			label = paste0(capitalize(gsub("_", " ", colnames(samples_data)[i])), " :"),
					                	choices = c("All", sort(unique(unlist(strsplit(as.character(samples_data[,i]), ","))))),
					                	width = "200px",
					                	selected = "All"
					            	)
					      		)
					      	}
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
	## Genes Filters
	output$genes_filters <- renderUI({
		genes_data_table <- genes_data_table()
		tagList(
			bsCollapsePanel(
				title = h3("Genes Metadata Filters"),
				fluidRow(
					lapply(1:ncol(genes_data_table), function(i) {
						if (tolower(colnames(genes_data_table)[i]) != "description") {
							column(3,
								selectInput(
				        			inputId = colnames(genes_data_table)[i],
				        			label = paste0(capitalize(gsub("_", " ", colnames(genes_data_table)[i])), " :"),
				                	choices = c("All", sort(unique(unlist(strsplit(as.character(genes_data_table[,i]), ","))))),
				                	selected = "All",
				                	width = "200px"				                
								)
							)
						}
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
	## Graphs Panel
	output$graphs_tab <- renderUI({
		if(!is.null(tpms_data_table()) && !is.null(genes_data_table()) && !is.null(samples_data_table())){
			tagList(
				bsCollapsePanel(
			    	title = h3("Graphs"),
			    	tabsetPanel(
			    		type = "tabs",
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
					    	fluidRow(
					    		column(12,
						    		checkboxInput(
						    			inputId = "use_replicats",
						    			label = "Mean by replicats",
										value = FALSE
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
			    		),
			    		tabPanel(
			    			title = "Heatmap",
			    			br(),
			    			fluidRow(
								# Color palette
								selectInput(
									inputId = "color",
									label = "Color palette :",
									choices = c(RedBlue="RdYlBu","YellowBlue","Blues"),
									selected = "RedBlue"
								)
			    			),
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
			    		)
			    	)
		    	)
			)
		}
	})
	# Boxplot selected metadata
	output$metadata_sample_x <- renderUI({
		samples_data <- samples_data_table()
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
		genes_data <- genes_data_table()
		genes_data_columns <- c(colnames(genes_data[colnames(genes_data) != "description"]))
		tagList(
			selectInput(
				inputId = "meta_gene_x",
				label = NULL,
				choices = genes_data_columns,
				width = "180px"
			)
		)
	})
	output$metadata_gene_col <- renderUI({
		genes_data <- genes_data_table()
		genes_data_columns <- c(colnames(genes_data[colnames(genes_data) != "description"]))
		tagList(
			selectInput(
				inputId = "meta_gene_col",
				label = NULL,
				choices = subset(genes_data_columns, !(genes_data_columns %in% input$meta_gene_x)),
				width = "180px"
			)
		)
	})
	# Dotplot selected samples
	output$dotplot_sample_one <- renderUI({
		genes_data <- genes_data_table()
		final_table <- final_table()
		tagList(
			selectInput(
				inputId = "dotplot_sample_x",
				label = NULL,
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
				label = NULL,
				choices = subset(colnames(final_table[,-(1:ncol(genes_data))]), !(colnames(final_table[,-(1:ncol(genes_data))]) %in% input$dotplot_sample_x )),
				width = "180px"
			)
		)
	})
	## Sample List
	output$samples_id_filter <- renderUI({
    	samples_data <- samples_data_table()
    	wellPanel(
    		style = paste0("overflow-y:auto; max-height: 650px"),
    		HTML("<h4><b>Sample id :</h4></b>"),
			hr(),
			actionButton(
				inputId = "select_all",
				label = "Select all"
			),
			actionButton(
				inputId = "unselect_all",
				label = "Unselect all"
			),
	    	checkboxGroupInput(
	    		inputId = "sample_id",
	    		label = NULL,
	           	choices = as.character(samples_data[,1][order(nchar(samples_data[,1]), samples_data[,1])]),
	           	selected = as.character(samples_data[,1][order(nchar(samples_data[,1]), samples_data[,1])]),
	        	width = "200px"
			)
		)
    })


	## Events
	# Reset Samples Filters Values
	observeEvent(input$reset_samples_values, {
		samples_data <- samples_data_table()
		lapply(1:ncol(samples_data), function(i){
			if (!(tolower(colnames(samples_data)[i]) %in% sample_col_blacklist)) {
				if (tolower(colnames(samples_data)[i]) != "sample_id") {
		        	updateSelectInput(
		        		session = session, 
		        		inputId = colnames(samples_data)[i],
		        		selected = "All"
		        	)
		        }
		    }			
		})
	})
	# Reset Genes Filters Values
	observeEvent(input$reset_genes_values, {
		genes_data_table <- genes_data_table()
		lapply(1:ncol(genes_data_table), function(i){
        	updateSelectInput(
        		session = session, 
        		inputId = colnames(genes_data_table)[i],
        		selected = "All"
        	)
		})
  	})
  	# Reset Files
  	observeEvent(input$reset_files, {
  		genes_list(NULL)
  		reset('files_panel')
  	})
	# Update Samples List (Select All)
	observeEvent(input$select_all, {
		samples_data <- samples_data_table()
    	updateCheckboxGroupInput(
    		session = session,
			inputId = "sample_id",
          	choices = as.character(samples_data[,1][order(nchar(samples_data[,1]), samples_data[,1])]),
          	selected = as.character(samples_data[,1][order(nchar(samples_data[,1]), samples_data[,1])])
    	)
	})
	# Update Samples List (Unselect All)
	observeEvent(input$unselect_all, {
		samples_data <- samples_data_table()
    	updateCheckboxGroupInput(
    		session = session,
			inputId = "sample_id",
          	choices = as.character(samples_data[,1][order(nchar(samples_data[,1]), samples_data[,1])]),
          	selected = character(0)
    	)
	})	


	## Render
	# DataTable
	output$table <- renderDataTable({
    	samples_metadata <- samples_data_table()
		genes_data_table <- genes_data_table()
		tpms_data <- tpms_data_table()

		withProgress(message = 'Making table', value = 0, {
			incProgress(1/5, detail = paste("Apply samples filters"))
			# Apply samples filters
			samples_data <- lapply(1:ncol(samples_metadata), function(i){
				if (!(tolower(colnames(samples_metadata)[i]) %in% sample_col_blacklist)) {
					if (tolower(colnames(samples_metadata)[i]) == "sample_id") {
						samples_data <- subset(samples_metadata, samples_metadata[,i] %in% input$sample_id)
					} else {
						if (input[[colnames(samples_metadata)[i]]] != "All") {
						samples_data <- samples_metadata[samples_metadata[,i] %like% input[[colnames(samples_metadata)[i]]],]	
						}
					}
				}
			})
			samples_data <- Reduce(intersect, Filter(Negate(is.null), samples_data))

			incProgress(1/5, detail = paste("Update other samples filters"))				
			# Update other samples filters
			lapply(1:ncol(samples_data), function(i){
				if (!(tolower(colnames(samples_data)[i]) %in% sample_col_blacklist)) {
					if (tolower(colnames(samples_data)[i]) != "sample_id") {
						if (input[[colnames(samples_data)[i]]] == "All") {
						    updateSelectInput(
						       	session = session, 
						       	inputId = colnames(samples_data)[i],
						       	choices = c("All", sort(unique(unlist(strsplit(as.character(samples_data[,i]), ","))))),
						       	selected = "All"
				    		)
						}
					}
				}
			})

	    	# Merge TPMS and genes metadata files according to filters
		    gene_id <- tpms_data[1] # Supposing gene_id is the 1st column
		    filtered_tpms_data <- cbind(gene_id, tpms_data[colnames(tpms_data) %in% samples_data[,1]]) # Supposing sample_id is the 1st column
		    merged_genes_tpms <- merge(genes_data_table, filtered_tpms_data, by=colnames(genes_data_table[1]))
		    final_table <- merged_genes_tpms

			incProgress(1/5, detail = paste("Apply genes filters"))
		    # Apply genes filters
		    final_table <- lapply(1:ncol(genes_data_table), function(i){
				if (!(tolower(colnames(genes_data_table)[i]) %in% gene_col_blacklist)) {
					if (input[[colnames(genes_data_table)[i]]] != "All") {
						final_table <- merged_genes_tpms[merged_genes_tpms[,i] %like% input[[colnames(genes_data_table)[i]]],]
					} else {
						final_table <- merged_genes_tpms
					}
				}
			})
			final_table <- Reduce(intersect, Filter(Negate(is.null), final_table))

			incProgress(1/5, detail = paste("Update other genes filters"))				
			# Update other genes filters
			lapply(1:ncol(genes_data_table), function(i){
				if (!(tolower(colnames(genes_data_table)[i]) %in% gene_col_blacklist)) {
			    	if (input[[colnames(genes_data_table)[i]]] == "All") {
					    updateSelectInput(
					       	session = session, 
					       	inputId = colnames(final_table)[i],
					       	choices = c("All", sort(unique(unlist(strsplit(as.character(final_table[,i]), ","))))),
					       	selected = "All"
			    		)
					}
				}
			})

	    	# Update table by gene list file
	    	genes_list <- genes_list()
	    	if (length(genes_list) > 0) {
	    		final_table <- subset(final_table, final_table[,1] %in% genes_list[[1]])	
	    	}

		    incProgress(1/5, detail = paste("Building table"))
    	})

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
  			lengthMenu = list(c(10, 25, 50, 100, 200, -1),list("10","25","50","100","200","all")),
            scrollX = TRUE,
            buttons = list(
            	'copy', 'csv', 'excel', 'print',
            	list(extend = "pdf",
            		pageSize="A4",
            		filename = "table",
            		header=TRUE,
            		title=NULL,
            		orientation="landscape"
            	)
            )
        )
	)
  	# Boxplot
	output$boxplot <- downloadHandler (		
		filename = function() {
			paste0("boxplot.", tolower(input$boxplot_ext))
		},
		content = function(file){
			final_table <- final_table()
			samples_data <- samples_data_table()

			boxplot_data<-final_table[!(colnames(final_table) %in% colnames(genes_data_table()))]

			if (input$use_replicats == TRUE) {
				# Make mean table from replicats
				rows_mean <- lapply(1:ncol(boxplot_data), function(i){
					rows_mean <- rowMeans(boxplot_data[,c(samples_data[i,"sample_id"], samples_data[i,"replicats"])])
				})
				names(rows_mean) <- samples_data[,"sample_id"]
				boxplot_data <- as.data.frame(list.cbind(rows_mean))		
			}

			if (input$metadata == "Genes") {
				# Table for genes metadata
				genes_for_boxplot <- cbind(final_table[input$meta_gene_x],final_table[input$meta_gene_col], boxplot_data)
				melted_data<-melt(genes_for_boxplot, id=c(as.character(input$meta_gene_x),as.character(input$meta_gene_col)))
	
				buildBoxplot(melted_data, file, ext = input$boxplot_ext, x = input$meta_gene_x , col = input$meta_gene_col)

			} else if (input$metadata == "Samples") {
				# Transpose table for samples metadata
				transpo_tpms <- data.frame(t(boxplot_data))
				factors <- sapply(transpo_tpms, is.factor)
				transpo_tpms[factors] <- lapply(transpo_tpms[factors], as.character)
				colnames(transpo_tpms) <- final_table[,1]

				samples_id <- rownames(transpo_tpms)
				samples_data <- samples_data[samples_data[["sample_id"]] %in% samples_id,]
				
				transpo_tpms <- data.frame(transpo_tpms, row.names=NULL)
				samples_for_boxplot <- cbind(samples_data[input$meta_sample_x],samples_data[input$meta_sample_col], transpo_tpms)
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
			buildDotplot(final_table, file, ext = input$dotplot_ext, x=input$dotplot_sample_x, y=input$dotplot_sample_y)
		}
	)
	# Heatmap
	output$heatmap <- downloadHandler (
		filename = function() {
			paste0("heatmap.", tolower(input$heatmap_ext))
		},
		content = function(file) {
			final_table <- final_table()
			tpms_table <- final_table[!(colnames(final_table) %in% colnames(genes_data_table()))]
			rownames(tpms_table) <- final_table[,1]
			tpms_matrix <- data.matrix(tpms_table)

			if(input$color == "YellowBlue"){
				color_palette <- colorRampPalette(c("#FFCE00","#FFFFFF","#6B8BA3"))(10)
			} else {
				color_palette <- brewer.pal(10,input$color)
			}

			buildHeatmap(tpms_matrix, file, ext = input$heatmap_ext, color = color_palette)
		}
  	)
}

shinyApp(ui, server)

# Création d'une colonne nom complet
samples_data_table$sname <- do.call(paste, c(samples_data_table[cols], sep=":"))
# Déplace la nouvelle colonne en première colonne
samples_data_table <- samples_data_table %>% select(sname, everything())
# Renommage des samples selon les nouveaux noms dans tpms
names(tpms_data_table) <- samples_data_table$sname[match(names(tpms_data_table), samples_data_table$sample_id)]
# Suppression de la colonne sample_id et renommage de la nouvelle colonne
samples_data_table$sample_id <- NULL
names(samples_data_table)[names(samples_data_table)=="sname"]<-"sample_id"
