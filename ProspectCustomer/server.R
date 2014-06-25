#palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
options(shiny.maxRequestSize=30*1024^2)
#options(shiny.error=browser)
Sys.setenv(DISPLAY=":9") 

 
shinyServer(function(input, output, session) { 
 
 library(party)
# library(randomForest)
 library("rpart")
 library(MASS)
 library(e1071) ## works for R2.13.x
 library(klaR)  ## works for R2.13.x
# library(RWeka) ## works for R2.13.x
 library(class) ## works for R2.13.x
     
  output$plot <- renderPlot({  
       inFile <- input$file1

       if (is.null(inFile))
          return(NULL)
    
    	ds <-read.csv(inFile$datapath, header=input$header, sep=input$sep )
     
        fit <- aov(as.matrix(ds[,-1])~as.factor(ds[,1]))
	summ <- summary(fit)
	
	##---------------------------------------------
	## Extract F-stats with corresponding var names
	nn <- sapply(strsplit(names(summ),' '), function(x){
	  x[3]
	})
	
	Fstats <- sapply(1:length(summ), function(i){
	  summ[[i]]$F[1]
	}) 
	
	##---------------------------------------------
	## Order the F-stats and extract top n (e.g. 10)
	
	nVars=20
	orderr = order(Fstats,decreasing=TRUE) 
	Fstats[orderr][1:nVars]
	nn[orderr][1:nVars]
	
	nVars=20
	orderr = order(Fstats,decreasing=TRUE) 
	Fstats[orderr][1:nVars]
	nn[orderr][1:nVars]
	
	par(las=2)
	par(mar=c(4,9.4,3,2))
	x1<-(Fstats[orderr][nVars:1])
	x2<-nn[orderr][nVars:1]
	barplot(x1,col="red",cex.names=0.8,
	        names=x2, horiz=TRUE, axes=TRUE,xlim=c(0,300),space=0,width=1,beside = TRUE,xlab="F-Statistic" , main="TOPN Variable")  
  })
  
   output$accurancy <- renderTable({
       inFile <- input$file1

       if (is.null(inFile))
          return(NULL)
       
       
    	dat <-read.csv(inFile$datapath, header=input$header, sep=input$sep )
    	#dat<-ds
    	dat <- dat[,c("RR_FLAG",input$variables)]
    	ind <- sample(2, nrow(dat), replace=TRUE, prob=c(0.8, 0.2)) #split data into two subsets: training (70%) and testing (30%).
	train.df <- dat[ind==2,]
	test.df <- dat[ind==1,] 
	#"DT", "Baysian","SVM","LDA","KNN","Randomforest"
        
         library(shiny)
	 library(party)
	 library(randomForest)
	 library("rpart")
	 library(MASS)
	 library(e1071) ## works for R2.13.x
	 library(klaR)  ## works for R2.13.x
	 #library("RWeka") ## works for R2.13.x
	 library(class) ## works for R2.13.x
 
        fits = reactive({
	     switch(input$models,
	     "DT" = ctree(as.factor(RR_FLAG)~., data=test.df),
	     "Baysian" = naiveBayes(as.factor(RR_FLAG)~., data=test.df),
	     "SVM" = svm(as.factor(RR_FLAG)~., data=test.df,kernel="radial", gamma=0.01, cost=10),
	     #"KNN" =IBk(as.factor(RR_FLAG)~. , data=test.df, control=Weka_control(K=20, X=T)),
	     "Randomforest" = randomForest(as.factor(RR_FLAG)~., data=test.df, ntree=100, proximity=TRUE),
	     "LDA" = lda(as.factor(RR_FLAG)~., data=test.df)
	      ) 
	     })
	     
	pp = reactive({
	    predict(fits(), train.df[,-1])
	    })       
	
	tab <- table (prd=pp(), true=train.df[,1])
	#tab <- rbind("","",round(sum(diag(prop.table(tab))),3))
	tab  
	 
  })
  
  # Show the first "n" observations 
  
})
