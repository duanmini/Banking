#Sys.setenv(DISPLAY=":9") 
shinyServer(function(input, output, session) {
 
   
 
  output$contents <- renderTable({ 
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found. 
    inFile <- input$file1
  
    if (is.null(inFile))
      return(NULL)
    
    ds <-read.csv(inFile$datapath, header=input$header, sep=input$sep )
    
    #ds<-read.csv(inFile$datapath, header=input$header, sep=input$sep ) 
     head(ds,input$Records)                          
  } )
  
  # Combine the selected variables into a new data frame
  
  
  output$plot2D <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)    
    ds<-read.csv(inFile$datapath, header=input$header, sep=input$sep )  
   
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
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)
     
     ds <-read.csv(inFile$datapath, header=input$header, sep=input$sep )
     #ds<-read.csv(input$infile,header=input$header,sep=input$sep)
     cl<-kmeans(ds,input$clusters)
     #plot3d()
     plot3d(ds, col=cl$cluster, size=6)       
     #clusters()$centers 
  })
  
  output$cluster <- renderTable({  
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)
    
    ds <-read.csv(inFile$datapath, header=input$header, sep=input$sep )
    #ds<-read.csv(input$infile,header=input$header,sep=input$sep)
    cl<-kmeans(ds,input$clusters)
    cl$centers 
    #summary(cl)
  })
})