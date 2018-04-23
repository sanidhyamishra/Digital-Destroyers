#
# This is a Alarm Flood Analysis Problem for predicting the root cause of Alarm FLood
#

# Loaded the Libraries 

library(shiny)
library(e1071)
library(dplyr)
library(caret)
library(ggplot2)
library(psych)
library(data.table)
library(sqldf)
library(reticulate)
library(graphics)
library(reshape2)
library(class)
library(FastKNN)
library(arulesViz)

# Define UI for random distribution app ----
library(shinythemes)
ui <- fluidPage(theme = shinytheme("darkly"),
                
                tags$head(
                  tags$style(HTML("
                                  @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                  #final_text {
                                  text-align: center;
                                  }
                                  h1 {
                                  font-family: 'Lobster', cursive;
                                  font-weight: 700;
                                  font-size: 60px;
                                  line-height: 1.1;
                                  color:#FFF;
                                  text-align: center;
                                  }
                                  
                                  "))
                ),
                headerPanel("Alarm Flood Analysis"),
                br(),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    fileInput("file1", "Upload the Test Data",
                              accept = ".zip"
                    )
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    tags$head(
                      tags$style(type='text/css', 
                                 ".nav-tabs {font-size: 16px} ")), 
                    br(),
                    # Output: Tabset w/ plot, summary, and table ----
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("Classification",br(),br(),
                               div(style="display:inline-block;width:32%;text-align: center;",""),
                               actionButton("ClassifyButton", "Classify True and False Flood",
                                            icon("arrow-circle-right"), 
                                            style="color: #fff; background-color: #033969; border-color: #2e6da4"),
                               br(),br(),plotOutput("classificationPlot")),
                      tabPanel("Model Accuracy",br(),
                               div(style="display:inline-block;width:32%;text-align: center;",""),
                               verbatimTextOutput("Confusion Matrix") ),
                      tabPanel("Similarity",
                               br(),br(),
                               div(style="display:inline-block;width:32%;text-align: center;",""),
                               actionButton("SimilarityButton", "Find the Similarity Within the TrueFloods",
                                            icon("arrow-circle-right"), 
                                            style="color: #fff; background-color: #033969; border-color: #2e6da4"),
                               br(),br(),
                               h3(textOutput("SimilarityMatrixText")),
                               uiOutput("matrix"),
                               h3(textOutput("SimilarityGraphicMatrixText")),
                               plotOutput("SimilarityColor")),
                      tabPanel("Clustering",
                               br(),br(),
                               div(style="display:inline-block;width:32%;text-align: center;",""),
                               actionButton("ClusterButton", "Create Clustering Diagram",
                                            icon("arrow-circle-right"), 
                                            style="color: #fff; background-color: #033969; border-color: #2e6da4"),
                               br(),br(),
                               plotOutput("plot"),
                               h3(textOutput("ClusterInfo")),
                               br(),
                               tableOutput("Clusttable")),
                      tabPanel("Association",
                               br(),br(),
                               div(style="display:inline-block;width:32%;text-align: center;",""),
                               actionButton("SequenceButton", "Generate the Sequence Graph",
                                            icon("arrow-circle-right"), 
                                            style="color: #fff; background-color: #033969; border-color: #2e6da4"),
                               br(),br(),
                               h3(textOutput("Sequence1Text")),
                               plotOutput("sequence"),
                               h3(textOutput("Sequence2Text")),
                               plotOutput("sequence2")),
                      tabPanel("Prediction TestData",br(),br(),
                               div(style="display:inline-block;width:32%;text-align: center;",""),
                               actionButton("TestClassifyButton", "Classify Whether it is a true Flood or not",
                                            icon("arrow-circle-right"), 
                                            style="color: #fff; background-color: #033969; border-color: #2e6da4"),
                               h3(textOutput("TestClassifyButton")),
                               div(style="display:inline-block;width:32%;text-align: center;",""),
                               actionButton("TestSimiButton", "Calculate Similarity",
                                            icon("arrow-circle-right"), 
                                            style="color: #fff; background-color: #033969; border-color: #2e6da4"),
                               br(),
                               br(),
                               uiOutput("testMatrix"),
                               br(),
                               h3(textOutput("RootCause"))
                               
                      )
                      
                      
                    )
                    
                  )
                )
)

# Define server logic required to build the application

server <- function(input, output) {
  
  # the code for uploading the data to test folder
  df_products_upload <- observeEvent(input$file1,{
    inFile <- input$file1
    print(inFile)
    if (is.null(inFile))
      return(NULL)
    print(getwd())
    file.copy(inFile$datapath, paste0("test/","test.csv"), overwrite = TRUE, recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE)
  })
 
  # variables to get buttons reactive values
  v <- reactiveValues(data = NULL)
  v1 <- reactiveValues(data1 = NULL)
  v2 <- reactiveValues(data2 = NULL)
  v3 <- reactiveValues(data3 = NULL)
  v4 <- reactiveValues(data4 = NULL)
  v5 <- reactiveValues(data5 = NULL)
  v6 <- reactiveValues(data6 = NULL)
  
  
  # observe event for Classify button
  
  observeEvent(input$ClassifyButton, {
    v$data <- "Classify True and False Flood"
  })
  
  # observe event for Similarity buttons
  
  observeEvent(input$SimilarityButton, {
    v1$data1 <- "Find the Similarity Within the TrueFloods"
  })
  
  # observe event for Clustering buttons
  
  observeEvent(input$ClusterButton, {
    v2$data2 <- "Create Clustering Diagram"
  })
  
  # observe event for Sequence buttons
  
  observeEvent(input$SequenceButton, {
    v3$data3 <- "Generate the Sequence Graph"
  })
  
  # observe event for Test Files buttons
  observeEvent(input$TestClassifyButton, {
    v4$data4 <- "Test Classify"
  })
  
  observeEvent(input$TestSimiButton, {
    v5$data5 <- "Simil Classify"
  })
  
  # observe event for Confusion matrix buttons
  observeEvent(input$ConfusionMatrix, {
    v6$data6 <- "confusion"
  })
  
  # common code for generating similarity matrix
  d <- reactive({
    library(reticulate)
    use_python("C:/Python27/")
    source_python("AMA/SeqAlgo.py")
    
    i<-1
    listData<-c()
    
    c<-list.files(path = "AMA/simi/new/", pattern = NULL, all.files = TRUE,full.names = TRUE, recursive = TRUE,ignore.case = TRUE, include.dirs = FALSE, no.. = FALSE)
    FloodNames <- c()
    for (i in 1:length(c)) 
    {
      nam <- paste("F", i, sep = "") 
      FloodNames[[i]] <- nam
    }
    for (file in c)
    {
      firstData <- read.csv(file, header=TRUE, sep=",")
      # pre-allocate a list and fill it with a loop
      x1.list <- vector("list", nrow(firstData))
      for (i in 1:nrow(firstData)) 
      {
        str1 <- paste(firstData[i,5],firstData[i,6])
        x1.list[[i]] <- str1
      }
      for(otherfiles in c)
      {
        othersData <- read.csv(otherfiles, header=TRUE, sep=",")
        # pre-allocate a list and fill it with a loop
        y1.list <- vector("list", nrow(othersData))
        for (i in 1:nrow(othersData))
        {
          str1 <- paste(othersData[i,5],othersData[i,6])
          y1.list[[i]] <- str1
        }
        listData<-c(listData,calltoMain(x1.list,y1.list))
        i<-i+1
      }
    }
    y <- matrix(listData, nrow=length(c), ncol=length(c), byrow=TRUE,dimnames = list(FloodNames,FloodNames))
    #rownames(y)<-c("F1","F2","F3")
    #rownames(y) <- rownames(y, do.NULL = FALSE, prefix = "F")
    return(y)
  })

  # for Rendering the Table
  output$matrix <- renderTable(
    {
      if (is.null(v1$data1)) return()
      print(v1$data1)
      if(v1$data1 =="Find the Similarity Within the TrueFloods")
      {
        d()
      }
    })
  
  # for plotting the clustering diagram
  output$plot <- renderPlot(
    {
      if (is.null(v2$data2)) return()
      print(v2$data2)
      
      if(v2$data2 =="Create Clustering Diagram")
      {
        
        library(graphics)
        y <-d()
        dist <- as.dist(y)
        
        # Hierarchical clustering using Complete Linkage
        clusters <- hclust(dist, method = "complete" )
        # Plot the obtained dendrogram
        # op = par(bg = "#DDE3CA")
        plot(clusters,col = "#487AA1", col.main = "#45ADA8", 
             lwd = 3, lty = 3, sub = "", hang = -1)
        # add axis
        axis(side = 2, at = seq(0, 400, 100), col = "#F38630", labels = FALSE, 
             lwd = 2)
        # add text in margin
        mtext(seq(0, 400, 100), side = 2, at = seq(0, 400, 100), line = 1, 
              col = "#A38630", las = 2)
      }
      
    })
  
  # for plotting the colorful similarity matrix
  output$SimilarityColor <- renderPlot(
    {
      if (is.null(v1$data1)) return()
      print(v1$data1)
      if(v1$data1 =="Find the Similarity Within the TrueFloods")
      {
        library(reshape2)
        library(ggplot2)
        longData<-melt(d())
        
        ggplot(data = longData, aes(x=Var1, y=Var2, fill=value)) + 
          geom_tile() + scale_fill_gradient(low="grey90", high="red")
      }
    }
  )
  
  # for Rendering the CLust Table
  output$Clusttable <- renderTable(
    {
      if (is.null(v2$data2)) return()
      print(v2$data2)
      
      if(v2$data2 =="Create Clustering Diagram")
      {
        
        library(graphics)
        y <-d()
        dist <- as.dist(y)
        
        # Hierarchical clustering using Complete Linkage
        clusters <- hclust(dist, method = "complete" )
        
        k <- 3
        clusterCut <- cutree(clusters, k)
        
        c<-list.files(path = "AMA/simi/new/", pattern = NULL, all.files = TRUE,full.names = TRUE, recursive = TRUE,ignore.case = TRUE, include.dirs = FALSE, no.. = FALSE)
        
        x1.list <- NULL
        for (i in 1:length(c)) 
        {
          nam <- paste("F", i, sep = "") 
          x1.list[[i]] <- nam
        }
        
        t <-table(clusterCut, x1.list)
        y1<-as.matrix(t)
        return(y1)
        
      }
      
    })
  
  # Output Text 
  output$SimilarityMatrixText <- renderText(
    {
      if(!is.null(v1$data1) && v1$data1 =="Find the Similarity Within the TrueFloods")
      {
        "Similarity Matrix Representation"
      }
    }
  )
  
  output$Sequence1Text <- renderText(
    {
      if(!is.null(v3$data3) && v3$data3 =="Generate the Sequence Graph")
      { 
        "Graph of Association for Cluster1"
      }
    }
  )
  
  output$Sequence2Text <- renderText(
    {
      if(!is.null(v3$data3) && v3$data3 =="Generate the Sequence Graph")
      {
        "Graph of Association for Cluster2"
      }
    }
  )
  
  output$"SimilarityGraphicMatrixText" <- renderText(
    {
      if(!is.null(v1$data1) && v1$data1 =="Find the Similarity Within the TrueFloods")
      {
        "Similarity Matrix Graphical Representation"
      }
    }
  )
  
  output$"ClusterInfo" <- renderText(
    {
      if(!is.null(v2$data2) && v2$data2 =="Create Clustering Diagram")
      {
        "Cluster Information"
      }
    }
  )
  
  output$"TestClassifyButton" <- renderText(
    {
      if (is.null(v4$data4)) return()
      print(v4$data4)
      if(v4$data4 =="Test Classify")
      {

        featureSummery <- function(srcFolder){
          library(sqldf)
          c<-list.files(path = srcFolder, pattern = NULL, all.files = TRUE,full.names = TRUE, recursive = TRUE,ignore.case = TRUE, include.dirs = FALSE, no.. = FALSE)
          
          print(c)
          for (file in c){
            MyData <- read.csv(file, header=TRUE, sep=",")
            MyData$mysequence <- paste(MyData$Alarm.Tag,MyData$Alarm.Id)
            df<-sqldf("SELECT count(distinct(mysequence))*100/count(*) FROM MyData")
            names(df) <- NULL
            print(df)
            start_Time<-paste(MyData$Begin.Date,MyData$Begin.Time)
            start_Date_Time<-start_Time[1]
            
            end_Time<-paste(MyData$End.Date,MyData$End.Time)
            
            end_Date_Time<-end_Time[nrow(MyData)]
            duration<-strptime(end_Date_Time, "%m/%d/%Y %H:%M:%OS")-strptime(start_Date_Time, "%m/%d/%Y %H:%M:%OS")
            df[,2]<-as.numeric(duration)
            if (df[,1]>=60) {
              df[,3]<-"TRUE Flood"
              return("True Flood")
              
            }
            else {
              print("false flood")
              return("False Flood")
              df[,3]<-"FALSE Flood"
              # if (file.exists(file)) {
              #   file.remove(file)
              # }
            }
            
           
          }
        
        }
        
        testdf<- featureSummery("test")
        return(testdf)
      }
    }
  )
  
  # plot for classification 
  output$"classificationPlot" <- renderPlot(
    {
      if (is.null(v$data)) return()
      print(v$data)
      if(v$data =="Classify True and False Flood")
      {
        
        TrainingDestFile = "AMA/simi/MyTrainingData.csv"
        if(file.exists(TrainingDestFile)){
          file.remove(TrainingDestFile)
        }
        
        featureSummery <- function(srcFolder, destfile)
          {
          c<-list.files(path = srcFolder, pattern = NULL, all.files = TRUE,full.names = TRUE, recursive = TRUE,ignore.case = TRUE, include.dirs = FALSE, no.. = FALSE)
          
          for (file in c){
            MyData <- read.csv(file, header=TRUE, sep=",")
            MyData$mysequence <- paste(MyData$Alarm.Tag,MyData$Alarm.Id)
            df<-sqldf("SELECT count(distinct(mysequence))*100/count(*) FROM MyData")
            names(df) <- NULL
            
            start_Time<-paste(MyData$Begin.Date,MyData$Begin.Time)
            start_Date_Time<-start_Time[1]
            
            end_Time<-paste(MyData$End.Date,MyData$End.Time)
            
            end_Date_Time<-end_Time[nrow(MyData)]
            
            #duration<-strptime(end_Date_Time, "%Y/%m/%d %H:%M:%OS")-strptime(start_Date_Time, "%Y/%m/%d %H:%M:%OS")
            duration<-strptime(end_Date_Time, "%m/%d/%Y %H:%M:%OS")-strptime(start_Date_Time, "%m/%d/%Y %H:%M:%OS")
            
            df[,2]<-as.numeric(duration)
            if (df[,1]>60) {
              df[,3]<-1 
              #"TRUE Flood"
            } else {
              df[,3]<-0
              #"FALSE Flood"
              if (file.exists(file)) {
                file.remove(file)
              }
            }
            
            if(!file.exists(destfile)){
              names(df)<-c("Distinct Alarm Tags And ID","Duration in hours","Flood Label")
              
              write.table(df, file = destfile, sep = ",", append = TRUE, quote = FALSE, col.names = TRUE, row.names = FALSE)
              names(df) <- NULL
            }else{
              names(df) <- NULL
              write.table(df, file = destfile, sep = ",", append = TRUE, quote = FALSE, col.names = TRUE, row.names = FALSE)
              
            } 
          }
          return(read.csv(destfile, header=TRUE, sep=","))
        }
        
        #######################CONFUSION MATRIX LOGIC#######################
        # For importing data into an R data frame i.e. wine_df
        wine_df <- featureSummery("AMA/simi/new/",TrainingDestFile)
        
        ##########################DRAW PIE GRAPH###############
        
        trainingData <- read.csv("AMA/simi/MyTrainingData.csv", TRUE, sep = ",")
        data <- aggregate(trainingData$Flood.Label, 
                          by=list(trainingData$Flood.Label), FUN=sum)
        nrow(trainingData)
        data$x[[1]]<- nrow(trainingData)-data$x[[2]]
        if (data$Group.1[[1]]!=0){
          data$Group.1[[1]]<-"TRUE FLOOD"
          col =c("green","red")
        }else{
          data$Group.1[[1]]<-"FALSE FLOOD"
          data$Group.1[[2]]<- "TRUE FLOOD"
          col =c("red", "green")
        }
        
        pie(data$x, data$x, col =col,
            clockwise = TRUE, main = "Alarm Flood Status For Training Data")
        
        legend("topright", c("FALSE FLOOD","TRUE FLOOD"), 
               cex = 0.8, fill = c("red", "green"))
      }
    }
  )
  
  
  # observe event for Test Matrix buttons
  
  output$"testMatrix" <- renderTable(
    {
      if (is.null(v5$data5)) return()
      print(v5$data5)
      if(v5$data5 =="Simil Classify")
      {
        
        featureSummery <- function(srcFolder){
          library(sqldf)
          c<-list.files(path = srcFolder, pattern = NULL, all.files = TRUE,full.names = TRUE, recursive = TRUE,ignore.case = TRUE, include.dirs = FALSE, no.. = FALSE)
          
          print(c)
          for (file in c){
            MyData <- read.csv(file, header=TRUE, sep=",")
            MyData$mysequence <- paste(MyData$Alarm.Tag,MyData$Alarm.Id)
            df<-sqldf("SELECT count(distinct(mysequence))*100/count(*) FROM MyData")
            names(df) <- NULL
            print(df)
            start_Time<-paste(MyData$Begin.Date,MyData$Begin.Time)
            start_Date_Time<-start_Time[1]
            
            end_Time<-paste(MyData$End.Date,MyData$End.Time)
            
            end_Date_Time<-end_Time[nrow(MyData)]
            duration<-strptime(end_Date_Time, "%m/%d/%Y %H:%M:%OS")-strptime(start_Date_Time, "%m/%d/%Y %H:%M:%OS")
            df[,2]<-as.numeric(duration)
            if (df[,1]>=60) {
              df[,3]<-"TRUE Flood"
              return("True Flood")
              
            }
            else {
              print("false flood")
              return("False Flood")
              df[,3]<-"FALSE Flood"
              # if (file.exists(file)) {
              #   file.remove(file)
              # }
            }
            
          }
          
        }
        
        testdf<- featureSummery("test")
        print(testdf)
        if(testdf == "True Flood")
        {
          library(reticulate)
          use_python("C:/Python34/")
          source_python("AMA/SeqAlgo.py")
          
          i<-1
          listData<-c()
          
          c<-list.files(path = "AMA/simi/new/", pattern = NULL, all.files = TRUE,full.names = TRUE, recursive = TRUE,ignore.case = TRUE, include.dirs = FALSE, no.. = FALSE)
          d<-list.files(path = "test/", pattern = NULL, all.files = TRUE,full.names = TRUE, recursive = TRUE,ignore.case = TRUE, include.dirs = FALSE, no.. = FALSE)
          
          for (file in d)
          {
            firstData <- read.csv(file, header=TRUE, sep=",")
            # pre-allocate a list and fill it with a loop
            x1.list <- vector("list", nrow(firstData))
            for (i in 1:nrow(firstData)) 
            {
              str1 <- paste(firstData[i,5],firstData[i,6])
              x1.list[[i]] <- str1
            }
            for(otherfiles in c)
            {
              othersData <- read.csv(otherfiles, header=TRUE, sep=",")
              # pre-allocate a list and fill it with a loop
              y1.list <- vector("list", nrow(othersData))
              for (i in 1:nrow(othersData))
              {
                str1 <- paste(othersData[i,5],othersData[i,6])
                y1.list[[i]] <- str1
              }
              listData<-c(listData,calltoMain(x1.list,y1.list))
              i<-i+1
            }
          }
          y <- matrix(listData, nrow=1, ncol=length(c), byrow=TRUE)
          
        } 
      }
    }
  )
  
  output$"sequence" <- renderPlot(
    {
      if (is.null(v3$data3)) return()
      print(v3$data3)
      if(v3$data3 =="Generate the Sequence Graph")
      {
        
        library(plyr)
        
        df_groceries <- read.csv("AMA/simi/new/ALARM_LIST_ALARM_FLOODS_2016-12-01 00_00_00_2016-12-01 04_45_100 - Copy - Copy.csv")
        
        df_sorted <- df_groceries[order(df_groceries$Alarm.Priority),]
        df_sorted$Member_number <- as.numeric(df_sorted$Alarm.Priority)
        df_itemList <- ddply(df_groceries,c("Alarm.Priority"), 
                             function(df1)paste(df1$Alarm.Id,df1$Alarm.Tag,
                                                collapse = ","))
        df_itemList$Alarm.Type <- NULL
        
        colnames(df_itemList) <- c("itemList")
        
        write.csv(df_itemList,"ItemList.csv", row.names = TRUE)
        
        txn = read.transactions(file="ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);
        
        basket_rules <- apriori(txn,parameter = list(sup = 0.001, conf = 0.8,target="rules"))
        
        library(arulesViz)
        
        basket_rules <- head(sort(basket_rules, by="lift"), 10)
        
        
        plot(basket_rules, method="graph")
      }
    }
  )
  
  output$"sequence2" <- renderPlot(
    {
      if (is.null(v3$data3)) return()
      print(v3$data3)
      if(v3$data3 =="Generate the Sequence Graph")
      {
        
        library(plyr)
        
        df_groceries <- read.csv("AMA/simi/new/ALARM_LIST_ALARM_FLOODS_2016-12-01 00_00_00_2016-12-01 04_45_100.csv")
        
        df_sorted <- df_groceries[order(df_groceries$Alarm.Priority),]
        df_sorted$Member_number <- as.numeric(df_sorted$Alarm.Priority)
        df_itemList <- ddply(df_groceries,c("Alarm.Priority"), 
                             function(df1)paste(df1$Alarm.Id,df1$Alarm.Tag,
                                                collapse = ","))
        df_itemList$Alarm.Type <- NULL
        
        colnames(df_itemList) <- c("itemList")
        
        write.csv(df_itemList,"ItemList1.csv", row.names = TRUE)
        
        txn = read.transactions(file="ItemList1.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);
        
        basket_rules <- apriori(txn,parameter = list(sup = 0.001, conf = 0.8,target="rules"))
        
        library(arulesViz)
        
        basket_rules <- head(sort(basket_rules, by="lift"), 10)
        
        plot(basket_rules, method="graph")
      }
    }
  )
  
  # FOr rendering the Root Cause
  output$"RootCause" <- renderText(
    {
      if (is.null(v5$data5)) return()
      print(v5$data5)
      if(v5$data5 =="Simil Classify")
      {
        library(class)
        library(FastKNN)
        print("sani")
        print(d())
        file ="clusterResult.csv"
        
        prc <- read.csv(file,stringsAsFactors = FALSE)
        
        
        intrain <- createDataPartition(y = prc$label, p= 0.6, list = FALSE)
        training <- prc[intrain,]
        test <- prc[-intrain,]
        
        # KNN
        paste0("The Root Cause is ",knn_test_function(training, test, d(),prc$label, k = 3))
      }
    }
  )
  
  # For PRinting the confusion Matrix
  output$"Confusion Matrix" <- renderPrint(
    {
      # if (is.null(v6$data6)) return()
      # if(v6$data6 =="confusion")
      # {
      TestingDestFile = "MyTestingData.csv"
      TrainingDestFile = "MyTrainingData.csv"
      if(file.exists(TestingDestFile)){
        file.remove(TestingDestFile)
      }
      if(file.exists(TrainingDestFile)){
        file.remove(TrainingDestFile)
      }
      
      ##########################
      
      featureSummery <- function(srcFolder, destfile){
        library(sqldf)
        c<-list.files(path = srcFolder, pattern = NULL, all.files = TRUE,full.names = TRUE, recursive = TRUE,ignore.case = TRUE, include.dirs = FALSE, no.. = FALSE)
        
        for (file in c){
          MyData <- read.csv(file, header=TRUE, sep=",")
          MyData$mysequence <- paste(MyData$Alarm.Tag,MyData$Alarm.Id)
          df<-sqldf("SELECT count(distinct(mysequence))*100/count(*) FROM MyData")
          names(df) <- NULL
          
          start_Time<-paste(MyData$Begin.Date,MyData$Begin.Time)
          start_Date_Time<-start_Time[1]
          
          end_Time<-paste(MyData$End.Date,MyData$End.Time)
          
          end_Date_Time<-end_Time[nrow(MyData)]
          
          duration<-strptime(end_Date_Time, "%m/%d/%Y %H:%M:%OS")-strptime(start_Date_Time, "%m/%d/%Y %H:%M:%OS")
          
          df[,2]<-as.numeric(duration)
          if (df[,1]>=60) {
            df[,3]<-1 
            #"TRUE Flood"
          } else {
            df[,3]<-0
            #"FALSE Flood"
            if (file.exists(file)) {
              #print("It is a non interesting flood::::::::::")
              #file.remove(file)
            }
          }
          
          
          
          if(!file.exists(destfile)){
            names(df)<-c("Distinct Alarm Tags And ID","Duration in hours","Flood Label")
            
            write.table(df, file = destfile, sep = ",", append = TRUE, quote = FALSE, col.names = TRUE, row.names = FALSE)
            names(df) <- NULL
          }else{
            names(df) <- NULL
            
            write.table(df, file = destfile, sep = ",", append = TRUE, quote = FALSE, col.names = TRUE, row.names = FALSE)
            
          } 
        }
        return(read.csv(destfile, header=TRUE, sep=","))
      }
      
      #######################CONFUSION MATRIX LOGIC#######################
      # For importing data into an R data frame i.e. wine_df
      wine_df <- featureSummery("AMA/simi/new - Copy/",TrainingDestFile)
      testdf<- featureSummery("AMA/simi/test-data/", TestingDestFile)
      
      
      training <- wine_df
      names(testdf)<-names(wine_df)
      testing <- testdf
      
      
      #check dimensions of train & test set
      dim(training)
      dim(testing)
      
      # To check whether our data contains missing values or not
      anyNA(wine_df)
      
      training[["Flood.Label"]] = factor(training[["Flood.Label"]])
      
      # Train the knn model
      nb_fit <- naiveBayes(Flood.Label ~., data = training, method = "nb",
                      preProcess = c("center", "scale"),
                      tuneLength = 10)
      
      # Test the trained model
      test_pred <- predict(nb_fit, newdata = testing,na.action = na.pass)
      
      
      test<-testing$Flood.Label

      confusionMatrix(test,test_pred)
      
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

