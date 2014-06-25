  
shinyUI(pageWithSidebar(
  headerPanel('Prospective Customer'),
  sidebarPanel(
    #textInput("infile", "File Loading:", "E:/kmean.csv") ,
    
    fileInput('file1', 'Choose CSV File',
                accept=c('text/csv',  'text/comma-separated-values,text/plain', '.csv')),
      
      tags$hr(), 
    checkboxInput('header', 'Header', TRUE),
      
    radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
    #numericInput('Records', 'Record count', 20,  min = 1, max = 100)  ,
    
    #radioButtons("choice", "Predict type:",
    #               c("Prospective Custumer" = "RR_FLAG",
    #                 "Custumer Churning" = "CANCEL_FLAG") ) 	,
    tags$hr(),    
    
    selectInput("models", "Choose a classifying model:",  
	   choices = c("DT", "Baysian","SVM","Randomforest"),"DT"),
    tags$hr(),                      
    checkboxGroupInput("variables", "Variable Lists:",
	c("KHLX","KHXB","CSRQ","XLBH","KHRQ","KHJB","KZ_KYZJ","KZ_ZQSZ","KZ_JJSZ","KZ_ZZC","KZ_YMZZC","KZ_FPS_KHSZ","KZ_TZZLX","KZ_YXZX","KZ_BNJYL","KZ_BNYJ","KZ_SYJYL","KZ_SYYJ","KZ_ZJJYSJ","KZ_01","KZ_PJZC_KHQ","KZ_YKJE_KHQ","KZ_ZJFL","KZ_ZZL","KZ_KHLY","KZ_KHJZ","KZ_KHHYD","KZ_KHNX","KZ_YJFL","KZ_ZCFL","KZ_YKFL","KZ_TZPHFL","KZ_KHFJ","KZ_KHNXFL","KZ_FXCSNL","KZ_PJZC_2","KZ_ZRZJ_2","KZ_ZCZJ_2","KZ_PJZC_6","KZ_YKJE_6","KZ_PJZC_BN","KZ_PJZC_SN","KZ_MRZQZE_6","KZ_MRQZZE_6","KZ_MRJJZE_6","KZ_YJ_6","KZ_JYL_6","KZ_MRZE_6","KZ_MRGPZE_6","KZ_YKL_6","KZ_SNYJ","KZ_ZRZJ_3","KZ_ZCZJ_3","KZ_ZRGP_3","KZ_ZCGP_3","KZ_QCZC_3","KZ_QMZC_3","KZ_YKL_3","KZ_BYJYL","KZ_BYYJ","KZ_FWFJ","KZ_KHSJLY","KZ_GTKHID","KZ_02","KZ_SJYJL_AG","KZ_PJZC_4","KZ_PJZC_1","KZ_YJ_12","KZ_JYYYKJE","KZ_BYYK","KZ_SYYK","KZ_BYYJ_A","KZ_SYYJ_A","KZ_BNYJ_A","KZ_BYJYL_A","KZ_SYJYL_A","KZ_BNJYL_A","KZ_SNJYL","KZ_DQBH","KZ_KHDJB","KZ_BZYJ","KZ_XEXMH","KZ_SFXXLS","KZ_SFYXLS","KZ_SFZCH","KZ_FWYJ","KZ_FWYJL","KZ_SFYXH","KZ_SFQY","KZ_ZZC_RMB","TRZKZJ","ZC3","JYJE3","YK6" ),
	c("KZ_SFQY","TRZKZJ","KZ_FXCSNL","KZ_ZCFL","XLBH", "JYJE3","ZC3", "KHXB","KHJB","KZ_ZZL"))    
	                 
    #dat <- ds[,c("CANCEL_FLAG","KZ_02","KZ_01","KZ_KHJZ","ZC3","KZ_XEXMH","KZ_ZJFL","KZ_SFYXH","KZ_ZCFL", "YK6","KZ_SFZCH","KZ_ZJJYSJ","KZ_BZYJ")]
    
	                                     
                  
    #selectInput('xcol', 'X Variable', c("VALUE","VOLICITY","YEARS")) ,
    #selectInput('ycol', 'Y Variable', c("VALUE","VOLICITY","YEARS"), "YEARS"),
    #sliderInput('percentage', 'Training Dataset(%):',   min = 0, max = 1, value = 0.8, step= 0.1)
  ),
  mainPanel(
  
      h4("Importance of Variables"),
      plotOutput("plot"),
      
      h4("Accurancy"),
      tableOutput("accurancy") 
      #h4("Matrix"),
      #tableOutput("view") 
       
  )
))
