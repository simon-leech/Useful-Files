# UI.R
library(shiny)
library(DT)

ui<- fluidPage(
    title = 'DataTables Information',
    h1('Table'),
    fluidRow(
        column(6, DT::dataTableOutput('x1'))
    ),
    fluidRow(
        p(class = 'text-center', downloadButton('x3', 'Download Filtered Data'))
    )
)

server<- function(input, output, session) {
    
    # two columns of the mtcars data
    mtcars2 = mtcars
    
    # render the table (with row names)
    output$x1 = DT::renderDataTable(mtcars2, server = FALSE)
    
    # download the filtered data
    output$x3 = downloadHandler(filename=paste0("X","_","Y",".csv"), content = function(file) {
        s = input$x1_rows_all
        write.csv(mtcars2[s, , drop = FALSE], file)
    })
    
}

shinyApp(ui=ui, server=server)
