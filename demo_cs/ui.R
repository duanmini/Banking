shinyUI(pageWithSidebar(
  headerPanel('Customer Segmentation Demo'),
  sidebarPanel(
    textInput("infile", "File Loading:", "Loading sample dataset") ,
   #fileInput('file1', 'Choose CSV File',
   #            accept=c('text/csv',  'text/comma-separated-values,text/plain', '.csv')),
   #  
   #  tags$hr(), 
   #checkboxInput('header', 'Header', TRUE),
   #  
   #radioButtons('sep', 'Separator',
   #               c(Comma=',',
   #                 Semicolon=';',
   #                 Tab='\t'),
   #               ','),
    numericInput('Records', 'Record count', 20,  min = 1, max = 100)  ,
                     
    tags$hr(),               
    selectInput('xcol', 'X Variable', c("VALUE","VOLICITY","YEARS")) ,
    selectInput('ycol', 'Y Variable', c("VALUE","VOLICITY","YEARS"), "YEARS"),
    numericInput('clusters', 'Cluster count', 3,  min = 1, max = 9)
  ),
  mainPanel(
    tabsetPanel(type = "tabs", 
        tabPanel("Contents", tableOutput("contents")),
        tabPanel("Plot2D", plotOutput("plot2D")), 
        tabPanel("Summary", tableOutput("cluster")),
        tabPanel("Plot3D", plotOutput("plot3D"))  
        
    )
  )
))
