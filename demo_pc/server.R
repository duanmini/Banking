#palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
options(shiny.maxRequestSize=30*1024^2)
#options(shiny.error=browser)
Sys.setenv(DISPLAY=":9") 
#setwd("/srv/shiny-server")
dat<- read.csv("/srv/shiny-server/margin.csv",sep=",", header=T)
index <- dat$X
ind <- sample(2, nrow(dat), replace=TRUE, prob=c(0.8, 0.2))
 
test.df <- dat[ind==2,] 
#train.df <- dat[ind==1,] 
testindex <- test.df$X

dat<-dat[,-1] 
#test.df<-test.df[,-1] 

shinyServer(function(input, output, session) { 

 output$contents <- renderPlot({ 
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    #inFile <- input$file1
    #
    #if (is.null(inFile))
    #  return(NULL)
    #
    #dat<-read.csv(inFile$datapath, header=input$header, sep=input$sep,  quote=input$quote)
    #ds<- read.csv("/srv/shiny-server/cancel.csv",sep=",", header=T)
    #head(dat[,c("POTENCIAL",input$variables)],30)			 
        dat <- dat[,c(input$variables,"POTENCIAL")]
    	# ind <- sample(2, nrow(dat), replace=TRUE, prob=c(0.8, 0.2)) #split data into two subsets: training (70%) and testing (30%).
	train.df <- dat[ind==1,]
	test.df <- dat[ind==2,] 
	
	#testindex <- test.df$Index
	
	 cc <- ncol(dat)   ## No. of columns in the dataset
	 cols <- ifelse(train.df$POTENCIAL==0, 'red', 'blue')
	 # panel.cor function from R help manual of function pairs
	 panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
	 {
	     usr <- par("usr"); on.exit(par(usr))
	     par(usr = c(0, 1, 0, 1))
	     r <- abs(cor(x, y))
	     txt <- format(c(r, 0.123456789), digits=digits)[1]
	     txt <- paste(prefix, txt, sep="")
	     if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
	     text(0.5, 0.5, txt, cex = cex.cor * r)
	 }
	 
	 # look at training dataset
	 pairs(train.df, main=
	  "Pairs Plot of Train Dataset (red = NO; blue = YES)", 
	  lower.panel=panel.cor, col=cols, cex=1.2, pch=19)
	 				   
  } )
     
  output$plot <- renderPlot({  
     
        fit <- aov(as.matrix(dat[,-1])~as.factor(dat[,1]))
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
	
	nVars=input$topn
	orderr = order(Fstats,decreasing=TRUE) 
	Fstats[orderr][1:nVars]
	nn[orderr][1:nVars]
	 
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
  
   output$accurancy <- renderPlot({ 
    	dat <- dat[,c(input$variables,"POTENCIAL")]
    	#ind <- sample(2, nrow(dat), replace=TRUE, prob=c(0.9, 0.1)) #split data into two subsets: training (70%) and testing (30%).
	train.df <- dat[ind==1,]
	test.df <- dat[ind==2,] 
	#"DT", "Baysian","SVM","LDA","KNN","Randomforest"
        
         library(shiny)
	 library(party)
	 library(randomForest)
	 library("rpart")
	 library(MASS)
	 library(e1071) ## works for R2.13.x
	 library(klaR)  ## works for R2.13.x
	 library("RWeka") ## works for R2.13.x
	 library(class) ## works for R2.13.x
 
        fits = reactive({
	     switch(input$models,
	     "DT" = ctree(as.factor(POTENCIAL)~., data=train.df),
	     "Baysian" = naiveBayes(as.factor(POTENCIAL)~., data=train.df),
	     "SVM" = svm(as.factor(POTENCIAL)~., data=train.df,kernel="radial", gamma=0.01, cost=10),
	     "KNN" =IBk(as.factor(POTENCIAL)~. , data=train.df, control=Weka_control(K=20, X=T)),
	     "Randomforest" = randomForest(as.factor(POTENCIAL)~., data=train.df, ntree=100, proximity=TRUE),
	     "LDA" = lda(as.factor(POTENCIAL)~., data=train.df)
	      ) 
	     })
	     
	pp = reactive({
	    predict(fits(), test.df[,-ncol(dat)])
	    })       
	
	tab <- table (prd=pp(), true=test.df[,ncol(dat)])
	#tab <- rbind("","",round(sum(diag(prop.table(tab))),3))
	tab  
	
	Ratio =round(sum(diag(prop.table(tab))),3)
	
	cc <- ncol(dat)   ## No. of columns in the dataset
 	cols <- ifelse(test.df$POTENCIAL==0, 'red', 'blue')
 
	#testindex <- train.df$Index
	 
	 panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
	 {
	     usr <- par("usr"); on.exit(par(usr))
	     par(usr = c(0, 1, 0, 1))
	     r <- abs(cor(x, y))
	     txt <- format(c(r, 0.123456789), digits=digits)[1]
	     txt <- paste(prefix, txt, sep="")
	     if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
	     text(0.5, 0.5, txt, cex = cex.cor * r)
	 }
	 
	 i <- which(pp()!=test.df[,ncol(dat)])
	 cols <- ifelse(dat$POTENCIAL==0, 'red', 'blue')
	 cols <- ifelse(index %in% testindex[i], 'green', cols)
	 cexx <- ifelse(index %in% testindex[i], 1.5, 0.5)
	 pairs(dat[,-ncol(dat)], lower.panel=panel.cor, 
	       col=cols, pch=19, cex=cexx, main=paste(
	       "Machine Learning by ", input$models ," (Accurancy Rate =", round(Ratio,3)*100, "%)"))
       
	#output$acc<- round(sum(diag(prop.table(tab))),3)
  })
  
  # Show the first "n" observations 
  
})
