# Set default repo from CRAN
options(repos=structure(c(CRAN="https://cran.rstudio.com/")))
# Update installed packages
#update.packages(ask=FALSE, checkBuilt=TRUE)
# Install some packages
install.packages(c('shiny'))

# Load packages
library(shiny)
library(DT)

source("lib/merge_duplicated_data.R")

# Import files
tpms_data_table<-read.csv("test-data/TPMs-1_test.csv", sep="\t", header=TRUE, quote="")

#genes_data_file<-read.csv("test-data/genes_data_test.csv", sep="\t", header=TRUE, quote="", stringsAsFactors = FALSE)
genes_data_file<-read.csv("test-data/genes_data_test_duplicated.csv", sep="\t", header=TRUE, quote="", stringsAsFactors = FALSE)
genes_data_table<-merge_duplicated_data(genes_data_file)

samples_data_table<-read.csv("test-data/samples_data_test.csv", sep="\t", header=TRUE, quote="")


# User interface
ui <- bootstrapPage(
	includeCSS("static/css/styles.css"),
	fluidRow(
		column(2,
	      	fileInput(
	      		inputId = "tpms_file", 
	      		label = "Import TPMs File (.csv)",
	            multiple = FALSE,
	            accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
	        ),
	      	fileInput(
	      		inputId = "genes_data_file", 
	      		label = "Import Genes Data File (.csv)",
	            multiple = FALSE,
	            accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
	        ),
	      	fileInput(
	      		inputId = "sample_data_file", 
	      		label = "Import Samples Data File (.csv)",
	            multiple = FALSE,
	            accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
	        )	        
		),
		column(2,
			checkboxInput("header", "Header", TRUE),
			radioButtons("sep", "Separator",
                choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                selected = "\t"
            ),
		    radioButtons("quote", "Quote",
		        choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
		        selected = '"'
		    )
		),
		column(8,
			fluidRow(
				column(4,
					h5(strong("Sample Id :")),
					selectInput(
						inputId = "sample_id",
						label = NULL,
						choices = c("All", unique(as.character(samples_data_table$sample_id))),
						#multiple=TRUE,
						width = '200px'
					)
				),
				column(4,
					h5(strong("Strain :")),
					selectInput(
						inputId = "strain",
						label = NULL,
						choices = c("All", unique(as.character(samples_data_table$strain))),
						width = '200px'
					)
				),
				column(4,
					h5(strong("Phenotype :")),
					selectInput(
						inputId = "phenotype",
						label = NULL,
						choices = c("All", unique(as.character(samples_data_table$phenotype))),
						width = '200px'
					)
				)
			),
			fluidRow(
				column(4,
					h5(strong("Sex :")),
					selectInput(
						inputId = "sex",
						label = NULL,
						choices = c("All", unique(as.character(samples_data_table$sex))),
						width = '200px'
					)
				),
				column(4,
					h5(strong("Stage :")),
					selectInput(
						inputId = "stage",
						label = NULL,
						choices = c("All", unique(as.character(samples_data_table$stage))),
						width = '200px'
					)
				),
				column(4,
					h5(strong("Generation :")),
					selectInput(
						inputId = "generation",
						label = NULL,
						choices = c("All", unique(as.character(samples_data_table$generation))),
						width = '200px'
					)
				)
			),
			fluidRow(
				uiOutput("sampleId")
			)
		)
	),
	tags$hr(),
	fluidRow(
		dataTableOutput('table')
	)
)


# Server function
server <- function(input, output){

	output$table <- renderDataTable(
		{
			# Import file via fileInput
			if(FALSE){
				req(input$tpms_file)
				req(input$genes_data_file)
				req(input$sample_data_file)

				tpms_data_table <- read.csv(
					input$tpms_file$datapath,
	             	header = input$header,
	             	sep = input$sep,
	             	quote = input$quote
	            )

	            genes_data_table <- read.csv(
					input$genes_data_file$datapath,
	             	header = input$header,
	             	sep = input$sep,
	             	quote = input$quote
	            )

	            merged_data<-merge(tpms_data_table, genes_data_table, by="gene_id")

	            sample_data_table <- read.csv(
					input$genes_data_file$datapath,
	             	header = input$header,
	             	sep = input$sep,
	             	quote = input$quote
	            )
        	}

			samples_data<-samples_data_table
			tpms_data<-tpms_data_table
			if (input$sample_id != "All") {
		    	samples_data <- samples_data[samples_data$sample_id == input$sample_id,]
		    }
			if (input$strain != "All") {
		    	samples_data <- samples_data[samples_data$strain == input$strain,]
		    }
			if (input$phenotype != "All") {
		    	samples_data <- samples_data[samples_data$phenotype == input$phenotype,]
		    }
		    if (input$sex != "All") {
		    	samples_data <- samples_data[samples_data$sex == input$sex,]
		    }
		    if (input$stage != "All") {
		    	samples_data <- samples_data[samples_data$stage == input$stage,]
		    }
		    if (input$generation != "All") {
		    	samples_data <- samples_data[samples_data$generation == input$generation,]
		    }
		    gene_id<-tpms_data$gene_id
		    filtered_tpms_data <- cbind(gene_id, tpms_data[colnames(tpms_data) %in% samples_data$sample_id])
		    merged_data <- merge(genes_data_table, filtered_tpms_data, by="gene_id")
		    merged_data
		},
		rownames = FALSE,
	    extensions = c("Buttons", "FixedHeader"),
        options = list(
        	dom = 'Blfrtip',
        	order = list(0, 'asc'),
        	fixedHeader = TRUE,
        	pageLength = 25,
  			lengthMenu = c(10, 25, 50, 100, 200),
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
	)
}

shinyApp(ui, server)

if(FALSE){
	# Fonction pour remplacer les "," par des "." pour les valeurs de tpms
	substi<-function(x) {gsub("[,]",".",x) } 
	
	data_for_boxplot<-merged_data[,-c(2,3)]
	filtered_data<-melt(data_for_boxplot, id=c("gene_id","gene_group"))
	
	## BOX PLOT
	# Données et couleur
	ggplot(data = filtered_data, aes(x=gene_group, y=as.numeric(substi(value)), fill=gene_group)) #color=gene_group)) 
	# Boîtes et points
	+ geom_boxplot(aes(x=gene_group), outlier.colour="black", outlier.size=1)
	# Séparation en plusieurs graphes
	+ facet_wrap( ~ variable, scales="free") 
	# Palette de couleur
	+ scale_color_brewer(palette="Set1")

	## DOT PLOT
	ggplot(data = filtered_data, aes(x=gene_group, y=as.numeric(substi(value)), fill=gene_group))
	+ geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=1.2)
	+ facet_wrap( ~ variable, scales="free") + scale_color_brewer(palette="Set1")

	## HEATMAP
	# Remplacer les , par des . dans le fichier tpms
	melted_cormat<-melt(round(cor(tpms_data_table[,-c(1)]),2))
	ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) 
	# Séparateur de case
	+ geom_tile(color="white")
	#Gradient de couleur 
	+ scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab")
}