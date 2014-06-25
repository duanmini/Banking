#palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

options(shiny.maxRequestSize=30*1024^2)
#options(shiny.error=browser)
setwd("/srv/shiny-server")
ds<- read.csv("kmean.csv",sep=",", header=T)
#Sys.setenv(DISPLAY=":9") 

shinyServer(function(input, output, session) { 
 
  output$contents <- renderTable({ 
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found. 
    
     head(ds,input$Records)                          
  } )
  
  # Combine the selected variables into a new data frame
  
  
  output$plot2D <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
     
    selectedData <- reactive({
    ds[, c(input$xcol, input$ycol)]
   })
  
    clusters <- reactive({
    kmeans(selectedData(), input$clusters)
   })
   
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
     
  })

  output$plot3D <- renderPlot({  
    library(rgl) 
     
     #ds<-read.csv(input$infile,header=input$header,sep=input$sep)
     cl<-kmeans(ds,input$clusters)
     #plot3d()
     plot3d(ds, col=cl$cluster, size=6)       
     #clusters()$centers 
  })
  
  output$cluster <- renderTable({  
     
    #ds<-read.csv(input$infile,header=input$header,sep=input$sep)
    cl<-kmeans(ds,input$clusters)
    cl$centers 
    #summary(cl)
  })
})
