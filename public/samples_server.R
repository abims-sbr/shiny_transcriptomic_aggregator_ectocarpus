output$samples_table <- renderDT({
	samples_data_table()
},
	rownames = FALSE,
    extensions = c("Buttons"),
    options = list(
    	dom = 'Blfrtip',
    	order = list(0, 'asc'),
    	pageLength = 50,
			lengthMenu = list(c(10, 20, 50, 100, 200, -1),list("10", "20","50","100","200","all")),
        scrollX = TRUE,
        buttons = list(
            'copy', 'print',
            list(extend = "csv",pageSize="A4",filename = "table",header=TRUE,title=NULL,orientation="landscape"),
            list(extend = "excel",pageSize="A4",filename = "table",header=TRUE,title=NULL,orientation="landscape"),
            list(extend = "pdf",pageSize="A4",filename = "table",header=TRUE,title=NULL,orientation="landscape")
        )
    )
)