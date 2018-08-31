# Set default repo from CRAN
options(repos=structure(c(CRAN="https://cran.rstudio.com/")))
# Update installed packages
#update.packages(ask=FALSE, checkBuilt=TRUE)
# Install some packages
install.packages(c('shiny', 'shinyBS', 'gplots'))

# Load packages
library(shiny)
library(shinyBS)
library(DT)
library(gplots)
library(Hmisc)
library(reshape)

# Load functions
source("lib/merge_duplicated_data.R")
source("lib/read_tables.R")


# User interface
ui <- bootstrapPage(
	includeCSS("static/css/styles.css"),
	br(),
	fluidRow(
		# Import Files Panel
		column(3,
			bsCollapse(
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
		column(
			width = 12,
			dataTableOutput('table')
		)
	)
)


# Server function
server <- function(input, output, session){

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

    final_table <- reactiveVal(0)

	# Generate UI Samples Filters
	output$samples_filters <- renderUI({
		samples_data_table <- samples_data_table()
		tagList(
			bsCollapsePanel(
				title = h3("Samples Metadata Filters"),
				fluidRow(
			      	lapply(1:ncol(samples_data_table), function(i) {
			      		column(3,
							selectInput(
			        			inputId = colnames(samples_data_table)[i],
			        			label = paste0(capitalize(gsub("_", " ", colnames(samples_data_table)[i])), " :"),
			                	choices = c("All", sort(unique(samples_data_table[,i]))),
			                	width = "200px",
			                	selected = "All"
			            	)
			      		)
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
			                	choices = c("All", sort(unique(genes_data_table[,i]))),
			                	width = "200px",
			                	selected = "All"
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
			    			downloadButton(
					    		outputId = "heatmap",
					    		label = "Heatmap",
					    		width = "100%"
			    			)
			    		),
			    		tabPanel(
			    			title = "Boxplot",
			    			br(),
			    			fluidRow(
			    				column(6,
			    					h4("X Metadata :"),
					    			uiOutput("boxplot_meta_one")
					    		),
					    		column(6,
					    			h4("Y Metadata :"),
					    			uiOutput("boxplot_meta_two")
					    		)
				    		),			    			
					    	downloadButton(
					    		outputId = "boxplot",
					    		label = "Boxplot",
					    		width = "100%"
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

	# Boxplot selected metadata
	output$boxplot_meta_one <- renderUI({
		samples_data <- samples_data_table()
		genes_data <- genes_data_table()
		tagList(
			selectInput(
				inputId = "boxplot_meta1",
				label = NULL,
				choices = c(colnames(samples_data), colnames(genes_data)),
				width = "180px"
			)
		)
	})

	output$boxplot_meta_two <- renderUI({
		samples_data <- samples_data_table()
		genes_data <- genes_data_table()
		tagList(
			selectInput(
				inputId = "boxplot_meta2",
				label = NULL,
				choices = subset(c(colnames(samples_data), colnames(genes_data)), !(c(colnames(samples_data),colnames(genes_data)) %in% input$boxplot_meta1)),
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

	# DataTable
	output$table <- renderDataTable(
		{
        	samples_data <- samples_data_table()
			genes_data_table <- genes_data_table()
			tpms_data <- tpms_data_table()

			# Apply filters
			for ( i in 1:ncol(samples_data)) {
        		if (input[[colnames(samples_data)[i]]] != "All") {
				    samples_data <- samples_data[samples_data[i] == input[[colnames(samples_data)[i]]],]
				}
			}

			# Update other filters values
			for ( j in 1:ncol(samples_data)) {
				if ( input[[colnames(samples_data)[j]]] == "All") {
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
		    merged_genes_tpms <- merge(genes_data_table, filtered_tpms_data, by="gene_id")
		    final_table <- merged_genes_tpms

		    #Ajouter un filtre des lignes de merged_data selon les genes metadata
		    for ( i in 1:ncol(genes_data_table)){
		    	if (input[[colnames(genes_data_table)[i]]] != "All") {
			    	for ( j in 1:nrow(genes_data_table)) {
				    	final_table <- merged_genes_tpms[merged_genes_tpms[,i] == input[[colnames(genes_data_table)[i]]],]
				    }
				}
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
		filename = "heatmap.png",
		content = function(file){
			final_table <- final_table()
			tpms_data <- final_table[,-c(2:4)]

			tpms_table <- tpms_data[,-1]
			rownames(tpms_table) <- tpms_data[,1]

			png(file, width = 1500, height = 1000) #pdf(file)
			#print(heatmap(as.matrix(scale(tpms_table)), col=colorRampPalette(c("red","white","blue"))(10), xlab="Samples", ylab="Genes", main="Heatmap with hierarchical ascendant clustering from centered scaled TPMs"))
			print(
				heatmap.2(
					as.matrix(scale(tpms_table, center=TRUE, scale=TRUE)),
					col=colorRampPalette(c("red","white","blue"))(10), 
					scale="row", 
					margins=c(10,10), 
					srtCol=45,  
					xlab="Samples", 
					ylab="Genes", 
					main="Heatmap with hierarchical ascendant clustering from centered scaled TPMs"
				)
			)
			dev.off()
		}
  	)

  	# Boxplot
	output$boxplot <- downloadHandler (		
		filename = "boxplot.png",
		content = function(file){
			final_table <- final_table()
			genes_data_table <- genes_data_table()
			samples_data_table <- samples_data_table()

			boxplot_data<-final_table[,-c(2:length(genes_data_table))]
			boxplot_choice<-"samples"

			if (boxplot_choice == "genes"){
				# Table for genes metadata ===>>> Remplacer les id par les variables
				melted_data<-melt(boxplot_data, id=c("gene_id","gene_group"))
				# Ajouter les colonnes choisies
			} else if (boxplot_choice == "samples") {

				# Table for samples metadata
				transpo_tpms <- data.frame(t(boxplot_data))
				factors <- sapply(transpo_tpms, is.factor)
				transpo_tpms[factors] <- lapply(transpo_tpms[factors], as.character)
				colnames(transpo_tpms) <- transpo_tpms[1,]
				transpo_tpms <- transpo_tpms[-1,]

				sample_id <- rownames(transpo_tpms)
				transpo_tpms <- cbind(sample_id, data.frame(transpo_tpms, row.names=NULL))
				merged_samples <- merge(samples_data_table, transpo_tpms, by="sample_id")
				samples_for_boxplot <- merged_samples[,-c(1,4:6)]
				melted_data <- melt(samples_for_boxplot, id=c("strain","phenotype"))
			}

			png(file, width = 1500, height = 1000) #pdf(file)
			print(
				ggplot(
					data = melted_data,
					aes(
						x = as.character(phenotype),
						y = log2(as.numeric(value)),
						fill = as.character(strain)
					)
				)
				+ geom_boxplot(
					aes(x = as.character(phenotype)),
					outlier.colour = "black",
					outlier.size = 1
				)
				#+ facet_wrap( ~ variable, scales = "free")
				+ scale_color_brewer(palette = "Set1")
				+ ylab("log2(TPM)")
			)
			dev.off()
		}
  	)

  	# Dotplot
  	output$dotplot <- downloadHandler (	
		filename = "dotplot.png",
		content = function(file){
			final_table <- final_table()

			png(file, width = 1500, height = 1000) #pdf(file)
			print(
				ggplot(
					data = final_table,
					aes(
						x = get(input$dotplot_sample1),
						y = get(input$dotplot_sample2),
						fill = final_table[,1]
					)
				)
				+ geom_dotplot(
					binaxis = 'y',
					stackdir = 'center',
					dotsize = 0.5
				)
				+ scale_color_brewer(palette = "Set1")
				# + geom_label(aes(label = final_table[,1])
			)
			dev.off()
		}
	)
}

shinyApp(ui, server)


# Build graphs
if(FALSE){
	# Fonction pour remplacer les "," par des "." pour les valeurs de tpms
	substi<-function(x) {gsub("[,]",".",x) } 

	# Data for genes boxplot
	genes_for_boxplot<-merged_data[,-c(2,3)]
	genes_melted_data<-melt(genes_for_boxplot, id=c("gene_id","gene_group"))


	# Data for samples boxplot
	t_tpms <- data.frame(t(tpms))
	factors <- sapply(t_tpms, is.factor)
	t_tpms[factors] <- lapply(t_tpms[i], as.character)
	colnames(t_tpms) <- t_tpms[1,]
	t_tpms <- t_tpms[-1,]

	sample_id <- rownames(t_tpms)
	t_tpms <- cbind(sample_id, data.frame(t_tpms, row.names=NULL))
	merged_samples <- merge(samples_data_table, t_tpms, by="sample_id")
	samples_for_boxplot <- merged_samples[,-c(1,4:6)]
	samples_melted_data <- melt(samples_for_boxplot, id=c("strain","phenotype"))
	# Remplacer "strain" et "phenotype" par les variables choisies dans les inputs #


	## BOX PLOT
	# Données et couleur
	ggplot(data = filtered_data, aes(x=gene_group, y=as.numeric(substi(value)), fill=gene_group)) #color=gene_group)) 
	# Boîtes et points
	+ geom_boxplot(aes(x=gene_group), outlier.colour="black", outlier.size=1)
	# Séparation en plusieurs graphes
	+ facet_wrap( ~ variable, scales="free") 
	# Palette de couleur
	+ scale_color_brewer(palette="Set1")

	# Représente la distribution de TPMs de chaque groupe de gènes selon le sample
	ggplot(data = filtered_data, aes(x=gene_group, y=log2(as.numeric(substi(value))), fill=gene_group))+ geom_boxplot(aes(x=variable), outlier.colour="black", outlier.size=1) + scale_color_brewer(palette="Set1") + theme(axis.text.x = element_text(angle=45, hjust=1))
	# Représente la distribution des TPMs de chaque sample selon le groupe du gène
	ggplot(data = filtered_data, aes(x=variable, y=log2(as.numeric(substi(value))), fill=variable))+ geom_boxplot(aes(x=gene_group), outlier.colour="black", outlier.size=1) + scale_color_brewer(palette="Set1") + theme(axis.text.x = element_text(angle=45, hjust=1))
 	# Distribution des TPMs de chaque Strain selon le phénotype
	ggplot(data = samples_melted_data, aes(x=phenotype, y=log2(as.numeric(substi(value))), fill=as.character(strain)))+ geom_boxplot(aes(x=phenotype), outlier.colour="black", outlier.size=1) + scale_color_brewer(palette="Set1") + theme(axis.text.x = element_text(angle=45, hjust=1))


	## DOT PLOT
	ggplot(data = tpms, aes(x=sample1, y=sample2, fill=gene_id)) + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) + scale_color_brewer(palette="Set1") + geom_label(aes(label = gene_id))


	## HEATMAP
	ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value))+ geom_tile(color="white")	+ scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab")
}