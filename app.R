# Set default repo from CRAN
options(repos=structure(c(CRAN="https://cran.rstudio.com/")))
# Update installed packages
#update.packages(ask=FALSE, checkBuilt=TRUE)
# Install some packages
install.packages(c('shiny'))

# Load packages
library(shiny)
library(DT)

# Import files
tpms_data_table<-read.table("test-data/TPMs-1_test.csv", sep="\t", header=TRUE, quote="")
genes_data_table<-read.table("test-data/genes_data_test.csv", sep="\t", header=TRUE, quote="")
agg_data<-merge(tpms_data_table, genes_data_table, by="gene_id")

samples_data_table<-read.table("test-data/samples_data_test.csv", sep="\t", header=TRUE, quote="")

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

	            agg_data<-merge(tpms_data_table, genes_data_table, by="gene_id")

	            sample_data_table <- read.csv(
					input$genes_data_file$datapath,
	             	header = input$header,
	             	sep = input$sep,
	             	quote = input$quote
	            )
        	}

			samples_data<-samples_data_table
			data<-agg_data
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
		    filtered_data <- data[colnames(data) %in% samples_data$sample_id]
		    gene_id<-data$gene_id
		    cbind(gene_id, filtered_data)
		},
	    extensions = c("Buttons", "FixedHeader"),
        options = list(
        	dom = 'Blfrtip',
        	order = list(1, 'asc'),
        	fixedHeader = TRUE,
        	pageLength = 25,
  			lengthMenu = c(10, 25, 50, 100, 200),
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
	)
}

shinyApp(ui, server)