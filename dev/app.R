# Shiny packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
#library(shinyWidgets)
# Doc
library(markdown)
# Formatted text
library(Hmisc)
# Table
library(DT)
# Plots
library(ggplot2)
library(pheatmap)
library(RColorBrewer)
# Data manipulation
library(data.table)
library(reshape)
library(plyr)
#library(dplyr)
#library(rlist)


# Load configurations
source("conf.R", local = TRUE)
# Load functions
source("lib/read_tables.R")

# User interface
ui <- tagList(
	dashboardPage(
		dashboardHeader(title = HTML(paste0("Shiny Transcriptomic Aggregator - ", project)), titleWidth = 450),
		dashboardSidebar(
			sidebarMenu(id = "tabs",
				menuItem("Home", tabName = "home_tab", icon = icon("home")),
				menuItem("User guide", tabName = "guide_tab", icon = icon("info-circle")),
				menuItem("Table", tabName = "table_tab", icon = icon("table")),
				menuItem("Barplot", tabName = "barplot_tab", icon = icon("chart-bar")),
				menuItem("Boxplot", tabName = "boxplot_tab", icon = icon("chart-bar")),
				menuItem("Heatmap", tabName = "heatmap_tab", icon = icon("chart-bar")),
				menuItem("Import new data", tabName = "import_tab", icon = icon("file-import"))			)
		),
		dashboardBody(
			useShinyjs(),
			includeCSS("www/custom.css"),
			tabItems(
				source("home_ui.R", local = TRUE)$value,
				source("guide_ui.R", local = TRUE)$value,
				source("table_ui.R", local = TRUE)$value,
				source("barplot_ui.R", local = TRUE)$value,
				source("boxplot_ui.R", local = TRUE)$value,
				source("heatmap_ui.R", local = TRUE)$value,
				source("import_ui.R", local = TRUE)$value				
			)
		)
	),
	# Footer
	tags$footer(
		"Copyright (c) 2018-2019 - Station Biologique de Roscoff - ABiMS"
	)
)
# Server function
server <- function(input, output, session){

	# Variable Initialisation
	genes_data_table <- reactiveVal()
	samples_data_table <- reactiveVal()
	tpms_data_table <- reactiveVal()
	final_table <- reactiveVal()

	# TODO : Render error in case of missing input file
    # Assign default data
    if(exists("genes_data_input")){
	    if(!is.null(genes_data_input)){
	    	genes_data_table <- reactiveVal(value=getDataFrameFromFile(genes_data_input))
	    }
	}
	if(exists("samples_data_input")){
	    if(!is.null(samples_data_input)){
			samples_data_file <- getDataFrameFromFile(samples_data_input)
			if("private" %in% tolower(colnames(samples_data_file))){
				# If public instance, took of private sample data
				if(instance_tag == "public"){
					samples_data_file <- samples_data_file[toupper(samples_data_file[,"private"]) == "FALSE",]
				}
				# Remove private column from the table
				samples_data_file["private"] <- NULL
			}
			samples_data_table <- reactiveVal(value=samples_data_file)
		}
	}
	if(exists("tpms_input")){
	    if(!is.null(tpms_input)){
			tpms_data_table <- reactiveVal(getDataFrameFromFile(tpms_input))
    	}
    }

	observe ({
		# Remove private samples in tpms file
		tpms_data <- tpms_data_table()[, colnames(tpms_data_table()) %in% samples_data_table()[,"sample_id"]]
		tpms_data_table(cbind(tpms_data_table()["gene_id"], tpms_data))

		initial_table <- merge(genes_data_table(), tpms_data_table(), by=colnames(genes_data_table()["gene_id"]))
		final_table(initial_table)
	})

	source("import_server.R", local = TRUE)
	source("table_server.R", local = TRUE)
	source("barplot_server.R", local = TRUE)
	source("boxplot_server.R", local = TRUE)
	source("dotplot_server.R", local = TRUE)
	source("heatmap_server.R", local = TRUE)
}
shinyApp(ui, server)
