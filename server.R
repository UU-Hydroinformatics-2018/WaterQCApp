
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer(function(input, output) {
  
  url<-a("DWQ Buoy Station Data", href="https://wqdatalive.com/login")
  output$tab<-renderUI({
    tagList("URL link:",url)
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  data1 <- reactive({
    if(input$Load == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load
      my_data <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
      
      #Turn table from dataframe to matrix to format
      
      as.matrix(my_data)
      site_name <- colnames(my_data)
      siteid <- site_name [2]
      
      # Format Table for analysis
      
      colnames (my_data) <- NULL
      colnames (my_data) = my_data [1,]
      my_data <- my_data[-c(1),]
      site_units <- my_data[1,]
      data.frame(site_units)
      my_data <- my_data[-c(1),]
      names(my_data) <- gsub(" ",".",colnames(my_data))
      names(my_data) <- gsub("-",".",colnames(my_data))
    
      
      #Make numeric
      
      my_data$SiteLocation <- siteid
      my_data$Temperature<- as.numeric(my_data$Temperature)
      my_data$Sp.Cond <- as.numeric(my_data$Sp.Cond)
      my_data$pH.mV <- as.numeric(my_data$pH.mV)
      my_data$pH <- as.numeric(my_data$pH)
      my_data$Turbidity <- as.numeric(my_data$Turbidity)
      my_data$Chlorophyll <- as.numeric(my_data$Chlorophyll)
      my_data$Chlorophyll.RFU <- as.numeric(my_data$Chlorophyll.RFU)
      my_data$ODOSat <- as.numeric(my_data$ODOSat)
      my_data$ODO <- as.numeric(my_data$ODO)
      my_data$BGA.Phycocyanin.RFU <- as.numeric(my_data$BGA.Phycocyanin.RFU)
      
      #Format Time 
  
      #my_data[,1] <- as.POSIXct(my_data[,1],format="%m-%d-%Y %H:%M:%S")
     
      #QC Operations
      my_data$Temperature[35 < my_data$Temperature | my_data$Temperature < 0.5]<-NA
      
      my_data$Sp.Cond[50000 < my_data$Sp.Cond | my_data$Sp.Cond < 100]<-NA
      
      neg<-function(x) -x 
      my_data$pH.mV[200 < my_data$pH.mV | my_data$pH.mV < neg(250)]<-NA
      
      my_data$pH[my_data$pH < 2 | my_data$pH > 11]<-NA
      
      my_data$Turbidity[my_data$Turbidity > 1000 | my_data$Turbidity < 0.01]<-NA
      
      my_data$Chlorophyll[0 > my_data$Chlorophyll | my_data$Chlorophyll > 400]<-NA
      
      my_data$Chlorophyll.RFU[my_data$Chlorophyll.RFU >100 | my_data$Chlorophyll.RFU < 0 ]<-NA 
      
      my_data$ODOSat[my_data$ODOSat < 0 | my_data$ODOSat > 100]<-NA
      
      my_data$ODO[my_data$ODO < 0.5 | my_data$ODO > 15]<-NA
      
      my_data$BGA.Phycocyanin.RFU[0 > my_data$BGA.Phycocyanin.RFU | 100 < my_data$BGA.Phycocyanin.RFU]<-NA 
      
      
      #Add NAs if Blank
      for (i in nrow(my_data$Temperature)){
        if (is.null(my_data$Temperature)){
          my_data$Temperature <- NA
        }
      }
      
      for (i in nrow(my_data$Sp.Cond)){
        if (is.null(my_data$Sp.Cond)){
          my_data$Sp.Cond <- NA
        }
      }
      
      for (i in nrow(my_data$pH.mV)){
        if (is.null(my_data$pH.mV)){
          my_data$pH.mV <- NA
        }
      }
      
      for (i in nrow(my_data$pH)){
        if (is.null(my_data$pH)){
          my_data$pH <- NA
        }
      }
      
      for (i in nrow(my_data$Turbidity)){
        if (is.null(my_data$Turbidity)){
          my_data$Turbidity <- NA
        }
      }
      
      for (i in nrow(my_data$Chlorophyll)){
        if (is.null(my_data$Chlorophyll)){
          my_data$Chlorophyll <- NA
        }
      }
      
      for (i in nrow(my_data$Chlorophyll.RFU)){
        if (is.null(my_data$Chlorophyll.RFU)){
          my_data$Chlorophyll.RFU <- NA
        }
      }
      
      for (i in nrow(my_data$ODOSat)){
        if (is.null(my_data$ODOSat)){
          my_data$ODOSat <- NA
        }
      }
      
      for (i in nrow(my_data$ODO)){
        if (is.null(my_data$ODO)){
          my_data$ODO <- NA
        }
      }
      
      for (i in nrow(my_data$BGA.Phycocyanin.RFU)){
        if (is.null(my_data$BGA.Phycocyanin.RFU)){
          my_data$BGA.Phycocyanin.RFU <- NA
        }
      }
      
      #Drop unused Columns
      
      keep<- c("Time.(America/Boise)", "Temperature", "Sp.Cond","pH.mV",
               "pH","Turbidity","Chlorophyll","Chlorophyll.RFU",
               "ODOSat", "ODO", "BGA.Phycocyanin.RFU","SiteLocation")
      my_data = my_data[keep]
      
    })
    if(input$disp == "head") {
      return(head(my_data))
    }
    else {
      return(my_data)
    }
  })
  
  output$my_output_data <- renderTable({data1()},include.rownames=FALSE)  
  
  #Temperature Plot and Table
  
  output$plot_temp <- renderPlot({
    
    time1 <- as.POSIXct(data1()[,1],format="%m-%d-%Y %H:%M:%S")
    #time1 <- as.Date(time,format="%m-%d-%Y")
    temp1 <- as.numeric(data1()[,2])
    df <- data.frame(col1=time1,col2=temp1)
    
      
    ggplot(data=df,aes(x=time1,y=temp1,group = 1))+ 
      geom_point()+geom_smooth() + xlab("Time") + ylab("Temperature") + ggtitle("Time Series Comparison")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
  })
  
  temp_mod<-reactive({
    
    mod1 <- rbind("Site_Location" = data1()[2,12],
          "Para"= "Temperature",
          "5%" = round(quantile(data1()[,2],(0.05),na.rm =TRUE),2),
          "95%"= round(quantile(data1()[,2],na.rm = TRUE,(0.95)),2),
          "Mean" = round(mean(data1()[,2],na.rm=TRUE),2), 
          "Std Dev" = round(sd(data1()[,2],na.rm = TRUE),2))
    modtable <- data.frame(col1 = c("Site Location","Parameter","5% Quartile","95% Quartile","Mean","Std Dev"),
                           col2 = mod1)
    `colnames<-`(modtable,NULL)
    
  })
  
  output$table1 <- renderTable(temp_mod())
  
  # Sp Cond Plot and Table
  
  output$plot_sp <- renderPlot({
    
    time1 <- as.POSIXct(data1()[,1],format="%m-%d-%Y %H:%M:%S")
    #time1 <- as.Date(time,format="%m-%d-%Y")
    temp1 <- as.numeric(data1()[,3])
    df <- data.frame(col1=time1,col2=temp1)
    
    
    ggplot(data=df,aes(x=time1,y=temp1,group = 1))+ 
      geom_point()+geom_smooth() + xlab("Time") + ylab("Specific Conductivity uS/cm") + ggtitle("Time Series Comparison")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  sp_mod<-reactive({
    
    mod2 <- rbind("Site_Location" = data1()[2,12],
                  "Para"= "Specific Conductivity uS/cm",
                  "5%" = round(quantile(data1()[,3],(0.05),na.rm =TRUE),2),
                  "95%"= round(quantile(data1()[,3],na.rm = TRUE,(0.95)),2),
                  "Mean" = round(mean(data1()[,3],na.rm=TRUE),2), 
                  "Std Dev" = round(sd(data1()[,3],na.rm = TRUE),2))
    modtable <- data.frame(col1 = c("Site Location","Parameter","5% Quartile","95% Quartile","Mean","Std Dev"),
                           col2 = mod2)
    `colnames<-`(modtable,NULL)
    
  })
  
  output$table2 <- renderTable(sp_mod())
  
  # pH Plot and Table
  
  output$plot_pH <- renderPlot({
    
    time1 <- as.POSIXct(data1()[,1],format="%m-%d-%Y %H:%M:%S")
    #time1 <- as.Date(time,format="%m-%d-%Y")
    temp1 <- as.numeric(data1()[,5])
    df <- data.frame(col1=time1,col2=temp1)
    
    
    ggplot(data=df,aes(x=time1,y=temp1,group = 1))+ 
      geom_point()+geom_smooth() + xlab("Time") + ylab("pH") + ggtitle("Time Series Comparison")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  pH_mod<-reactive({
    
    mod3 <- rbind("Site_Location" = data1()[2,12],
                  "Para"= "pH",
                  "5%" = round(quantile(data1()[,5],(0.05),na.rm =TRUE),2),
                  "95%"= round(quantile(data1()[,5],na.rm = TRUE,(0.95)),2),
                  "Mean" = round(mean(data1()[,5],na.rm=TRUE),2), 
                  "Std Dev" = round(sd(data1()[,5],na.rm = TRUE),2))
    modtable <- data.frame(col1 = c("Site Location","Parameter","5% Quartile","95% Quartile","Mean","Std Dev"),
                           col2 = mod3)
    `colnames<-`(modtable,NULL)
    
  })
  
  output$table3 <- renderTable(pH_mod())
  
  # Turbidity Plot and Table
  
  output$plot_turb <- renderPlot({
    
    time1 <- as.POSIXct(data1()[,1],format="%m-%d-%Y %H:%M:%S")
    #time1 <- as.Date(time,format="%m-%d-%Y")
    temp1 <- as.numeric(data1()[,6])
    df <- data.frame(col1=time1,col2=temp1)
    
    
    ggplot(data=df,aes(x=time1,y=temp1,group = 1))+ 
      geom_point()+geom_smooth() + xlab("Time") + ylab("Turbidity NTU") + ggtitle("Time Series Comparison")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  turb_mod<-reactive({
    
    mod4 <- rbind("Site_Location" = data1()[2,12],
                  "Para"= "Turbidity NTU",
                  "5%" = round(quantile(data1()[,6],(0.05),na.rm =TRUE),2),
                  "95%"= round(quantile(data1()[,6],na.rm = TRUE,(0.95)),2),
                  "Mean" = round(mean(data1()[,6],na.rm=TRUE),2), 
                  "Std Dev" = round(sd(data1()[,6],na.rm = TRUE),2))
    modtable <- data.frame(col1 = c("Site Location","Parameter","5% Quartile","95% Quartile","Mean","Std Dev"),
                           col2 = mod4)
    `colnames<-`(modtable,NULL)
    
  })
  
  output$table4 <- renderTable(turb_mod())
  
  # Chlorophyll Plot and Table
  
  output$plot_chlor <- renderPlot({
    
    time1 <- as.POSIXct(data1()[,1],format="%m-%d-%Y %H:%M:%S")
    #time1 <- as.Date(time,format="%m-%d-%Y")
    temp1 <- as.numeric(data1()[,8])
    df <- data.frame(col1=time1,col2=temp1)
    
    
    ggplot(data=df,aes(x=time1,y=temp1,group = 1))+ 
      geom_point()+geom_smooth() + xlab("Time") + ylab("Chlorophyll RFU") + ggtitle("Time Series Comparison")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  chlor_mod<-reactive({
    
    mod5 <- rbind("Site_Location" = data1()[2,12],
                  "Para"= "Chlorophyll RFU",
                  "5%" = round(quantile(data1()[,8],(0.05),na.rm =TRUE),2),
                  "95%"= round(quantile(data1()[,8],na.rm = TRUE,(0.95)),2),
                  "Mean" = round(mean(data1()[,8],na.rm=TRUE),2), 
                  "Std Dev" = round(sd(data1()[,8],na.rm = TRUE),2))
    modtable <- data.frame(col1 = c("Site Location","Parameter","5% Quartile","95% Quartile","Mean","Std Dev"),
                           col2 = mod5)
    `colnames<-`(modtable,NULL)
    
  })
  
  output$table5 <- renderTable(chlor_mod())
  
  # DO Plot and Table
  
  output$plot_do <- renderPlot({
    
    time1 <- as.POSIXct(data1()[,1],format="%m-%d-%Y %H:%M:%S")
    #time1 <- as.Date(time,format="%m-%d-%Y")
    temp1 <- as.numeric(data1()[,10])
    df <- data.frame(col1=time1,col2=temp1)
    
    
    ggplot(data=df,aes(x=time1,y=temp1,group = 1))+ 
      geom_point()+geom_smooth() + xlab("Time") + ylab("Dissovled Oxygen mg/L") + ggtitle("Time Series Comparison")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  do_mod<-reactive({
    
    mod6 <- rbind("Site_Location" = data1()[2,12],
                  "Para"= "Dissovled Oxygen mg/L",
                  "5%" = round(quantile(data1()[,10],(0.05),na.rm =TRUE),2),
                  "95%"= round(quantile(data1()[,10],na.rm = TRUE,(0.95)),2),
                  "Mean" = round(mean(data1()[,10],na.rm=TRUE),2), 
                  "Std Dev" = round(sd(data1()[,10],na.rm = TRUE),2))
    modtable <- data.frame(col1 = c("Site Location","Parameter","5% Quartile","95% Quartile","Mean","Std Dev"),
                           col2 = mod6)
    `colnames<-`(modtable,NULL)
    
  })
  
  output$table6 <- renderTable(do_mod())
  
  # Phycocyanin Plot and Table
  
  output$plot_phy <- renderPlot({
    
    time1 <- as.POSIXct(data1()[,1],format="%m-%d-%Y %H:%M:%S")
    #time1 <- as.Date(time,format="%m-%d-%Y")
    temp1 <- as.numeric(data1()[,11])
    df <- data.frame(col1=time1,col2=temp1)
    
    
    ggplot(data=df,aes(x=time1,y=temp1,group = 1))+ 
      geom_point()+geom_smooth() + xlab("Time") + ylab("Pycocyanin RFU") + ggtitle("Time Series Comparison")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  phy_mod<-reactive({
    
    mod7 <- rbind("Site_Location" = data1()[2,12],
                  "Para"= "Pycocyanin RFU",
                  "5%" = round(quantile(data1()[,11],(0.05),na.rm =TRUE),2),
                  "95%"= round(quantile(data1()[,11],na.rm = TRUE,(0.95)),2),
                  "Mean" = round(mean(data1()[,11],na.rm=TRUE),2), 
                  "Std Dev" = round(sd(data1()[,11],na.rm = TRUE),2))
    modtable <- data.frame(col1 = c("Site Location","Parameter","5% Quartile","95% Quartile","Mean","Std Dev"),
                           col2 = mod7)
    `colnames<-`(modtable,NULL)
    
  })
  
  output$table7 <- renderTable(phy_mod())
  
  # Downloadable csv of selected dataset ----
  output$downloadData.csv <- downloadHandler(
    filename = function() {
      paste(data1()[2,12],".QC", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data1(), file, row.names = FALSE)
    }
  )
  
  
})