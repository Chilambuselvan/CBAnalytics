#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  #DataFrame for Closed + Open Opp  & selected Market
  reactdataAccountDB=reactive({
    AccDB%>%
      filter(Amount!=0)
    # ConsolidatedOpp%>%
    #   filter(Market.segment==input$MarSegChoose)%>%
    #   group_by(Region,Load,Speed)%>%
    #   summarise(cnt=sum(as.numeric(Quantity),na.rm = TRUE)) %>%
    #   arrange(desc(Region))
  })
  
  reactdataAccDB_MrktAnalysysByYearMon=reactive({
    AccDB%>%
      filter(TransactionYear==input$SelYear)%>%
      filter(TransactionMonth==input$SelMonth)
  })
  reactdataAccDB_MrktAnalysysByYear=reactive({
    AccDB%>%
      filter(TransactionYear==input$SelYear)
  })
  reactdataAccDB_CompReportByYear=reactive({
    AccDB%>%
      filter(TransactionYear==input$ComReportSelYear)
  })
    #Month Based on Year selected
  # reactdataAccDB_CompReportByYearMon
  observe({
    dt=AccDB %>%
      filter(TransactionYear==input$SelYear)
    # if (exists("ValTransactionMonth"))
    # {
    # rm(ValTransactionMonth)
    # }
    ValTransactionMonth=unique(dt$TransactionMonth[!is.na(dt$TransactionMonth)])
  })
   reactdataCon=reactive({
     ValTransactionMonth=AccDB%>%
       filter(TransactionYear==input$ValTransactionYear) %>%
       unique(TransactionMonth[!is.na(TransactionMonth)])
  })
  ######################################## Market Analysis ############################################
  output$HighIncome <- shinydashboard::renderValueBox({
    dt=reactdataAccDB_MrktAnalysysByYearMon() %>%
      filter(Category=='Income')
   
    shinydashboard::valueBox(
      paste0(max(dt$Amount), " EUR"), paste0("Highest Income ",dt$Company[match(max(dt$Amount),dt$Amount)]), icon = icon("list"),
      # paste0("2254", "EUR"), "Highest Income", icon = icon("list"),
      color = "purple"
    )
  })

  output$HighExp <- shinydashboard::renderValueBox({
    dt=reactdataAccDB_MrktAnalysysByYearMon()%>%
      filter(Category=='Expense') %>%
      filter(!Company %in% StandardExpenses )
    shinydashboard::valueBox(
      paste0(max(dt$Amount,na.rm = TRUE), " EUR"), paste0("Highest Expense ",dt$Company[match(max(dt$Amount),dt$Amount)]), icon = icon("list"),
      # paste0("2254", "EUR"), "Highest Income", icon = icon("list"),
      color = "purple"
    )
  })

  output$MonthlyTrack<-renderPlotly({
  
    #Case 1, Monthly analysis for month)
      dt=reactdataAccDB_MrktAnalysysByYearMon()%>%
        filter(Category=="Expense") %>%
        filter(!Company %in% StandardExpenses  ) %>%
        select(TransactionDate,Amount,Company) %>%
        group_by(TransactionDate)
   
      dt1=reactdataAccDB_MrktAnalysysByYearMon()%>%
        filter(Category=="Expense") %>%
        filter(!Company %in% StandardExpenses) %>%
        group_by(TransactionMonth) %>%
        summarise(Amt=sum(Amount,na.rm = TRUE))
      
   plot_ly() %>%
      add_trace(dt, labels=dt$Company, type='pie', values=dt$Amount) %>%
      layout(p, title=paste0("Expense for ",input$SelMonth, " ", input$SelYear, " = ",dt1$Amt ,'EUR'))
    
  })  
  output$MonthlyDayTrack<-renderPlotly({
  
    #Case 2,daily analysis for month)
      dt=reactdataAccDB_MrktAnalysysByYearMon()%>%
        filter(Category=="Expense") %>%
        filter(!Company %in% StandardExpenses  ) %>%
        select(TransactionDay,Amount,Company) %>%
        group_by(TransactionDay) %>%
        summarise(Amt=sum(Amount,na.rm = TRUE)) %>%
          arrange(desc(TransactionDay))
      
      dt$TransactionDay <- factor(dt$TransactionDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      
   plot_ly(dt) %>%   
     # add_lines(x=dt$TransactionDay,y=dt$Amt,name = input$SelMonth, line = list(shape = "spline")) %>%
     add_trace(x=dt$TransactionDay,y=dt$Amt,name = input$SelMonth, type='bar',text = dt$Amt,textposition = 'auto') %>%
     # add_trace(dt, x=dt$TransactionMonth,y=dt$Amount,type = 'bar',text = dt$Amount, textposition = 'auto',name=input$ComReportSelYear) %>%
     layout(title=paste0("Expense for ",input$SelMonth, " ", input$SelYear ," =",sum(dt$Amt)," EUR"))
   
    
  })
  output$YearlyDayTrack<-renderPlotly({
    
    #Case 3,daily analysis for Year)
    dt=reactdataAccDB_MrktAnalysysByYear()%>%
      filter(Category=="Expense") %>%
      filter(!Company %in% StandardExpenses  ) %>%
      select(TransactionDay,Amount,Company) %>%
      group_by(TransactionDay) %>%
      summarise(Amt=sum(Amount,na.rm = TRUE)) %>%
      arrange(desc(TransactionDay))
    
    dt$TransactionDay <- factor(dt$TransactionDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
    plot_ly(dt) %>%   
      # add_lines(x=dt$TransactionDay,y=dt$Amt,name = input$SelMonth, line = list(shape = "spline")) %>%
      add_trace(x=dt$TransactionDay,y=dt$Amt,name = input$SelMonth, type='bar',text = dt$Amt,textposition = 'auto') %>%
      # add_trace(dt, x=dt$TransactionMonth,y=dt$Amount,type = 'bar',text = dt$Amount, textposition = 'auto',name=input$ComReportSelYear) %>%
      layout(title=paste0("Expense for ",input$SelYear, " =",sum(dt$Amt)," EUR"))
    
    
  })
  ######################################## Overall Companies #####################################################
  output$tabOverAllMarket = renderDataTable({
    Dt=reactdataAccountDB()
    Dt%>%
      select(Company,Amount)%>%
      group_by(Company)%>%
      summarise(TotalAmount=sum(Amount,na.rm = TRUE))%>%
      # arrange(asce(TotalAmount))%>%
      
     #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
    #columns=c("Region","Load","Speed","cnt")
    datatable(Dt,filter="bottom",class = 'cell-border stripe',rownames = FALSE)
  })
  ##################################### Data View #####################################################
  output$tabOverAllCompanies = renderDataTable({
    Dt=reactdataAccountDB()
    
    #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
    #columns=c("Region","Load","Speed","cnt")
    datatable(Dt,filter="bottom",class = 'cell-border stripe',rownames = FALSE)
  })
  ##################################### Companies Report View  #####################################################
  output$CompanyReport_Std_MonthlyTrack<-renderPlotly({

    dt= AccDB %>%
      filter(TransactionYear==input$ComReportSelYear) %>%
      group_by(TransactionMonth) %>%
      filter(Category=="Expense") %>%
      # filter(Company == 'India Citibank') %>%
      filter(Company == input$ComReportSelStdComp) %>%
      select(TransactionMonth,Amount,Company) %>%
      summarise(Amount=sum(Amount))
    
     dt_By_Segment= AccDB %>%
       filter(TransactionYear==input$ComReportSelYear) %>%
       filter(Segment==input$ComReportSelSegment) %>%
       group_by(TransactionMonth) %>%
       filter(Category=="Expense") %>%
       select(TransactionMonth,Amount,Segment) %>%
       summarise(Amount=sum(Amount))
  
    plot_ly(dt_By_Segment) %>%   
      add_lines(x=dt_By_Segment$TransactionMonth,y=dt_By_Segment$Amount,name = input$ComReportSelSegment, line = list(shape = "spline")) %>%
      add_trace(dt, x=dt$TransactionMonth,y=dt$Amount,type = 'bar',text = dt$Amount, textposition = 'auto',name=input$ComReportSelYear) %>%
      layout(title=paste0("Standard Expense for ",input$ComReportSelStdComp, " ", input$ComReportSelYear))
    
  })
  
  output$CompanyReport_Irr_MonthlyTrack<-renderPlotly({
    
    dt=reactdataAccDB_CompReportByYear()%>%
      # AccDB %>%
      #   filter(TransactionYear==2018) %>%
      group_by(TransactionMonth) %>%
      filter(Category=="Expense") %>%
      # filter(Company == 'India Citibank') %>%
      filter(Company == input$ComReportSelIrrComp) %>%
      select(TransactionMonth,Amount,Company) %>%
      summarise(Amount=sum(Amount))
    
    plot_ly() %>%
      add_trace(dt, x=dt$TransactionMonth,y=dt$Amount,type = 'bar',text = dt$Amount, textposition = 'auto') %>%
      layout(p, title=paste0("Irregular Expense for ",input$ComReportSelIrrComp, " ", input$ComReportSelYear))
    
  })
  output$CompanyReport_Irr_DailyTrack<-renderPlotly({
      dt=reactdataAccDB_CompReportByYear()%>%
      filter(Category=="Expense") %>%
      filter(!Company %in% StandardExpenses  ) %>%
      filter(Company == input$ComReportSelIrrComp) %>%
      select(TransactionDay,Amount) %>%
      group_by(TransactionDay) %>%
      summarise(Amt=sum(Amount,na.rm = TRUE)) %>%
      arrange(desc(TransactionDay))
    
    dt$TransactionDay <- factor(dt$TransactionDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
    plot_ly(dt) %>%   
      # add_lines(x=dt$TransactionDay,y=dt$Amt,name = input$SelMonth, line = list(shape = "spline")) %>%
      add_trace(x=dt$TransactionDay,y=dt$Amt,name = input$SelMonth, type='bar',text = dt$Amt,textposition = 'auto') %>%
      # add_trace(dt, x=dt$TransactionMonth,y=dt$Amount,type = 'bar',text = dt$Amount, textposition = 'auto',name=input$ComReportSelYear) %>%
      layout(p, title=paste0("Irregular Expense for ",input$ComReportSelIrrComp, " ", input$ComReportSelYear," =",sum(dt$Amt)," EUR"))
      # layout(title=paste0("Expense for ",input$SelMonth, " ", input$SelYear ," =",sum(dt$Amt)," EUR"))
  
    
  })
  ##################################### Circling  #####################################################
  output$CyberportLaptop = renderGauge({
    
    ReturnDf=AccDB %>%
      filter(Segment=="Friends") %>%
      filter(str_detect(BookingText,'Ravi loan')) %>%
      summarise(Amt=sum(Amount))
   
    gauge(ReturnDf$Amt, min = 0, max = 569, symbol = 'EUR',label = paste("Laptop Cyberport"), gaugeSectors(
      success = c(450,569), warning = c(250, 450), danger = c(0, 250)
    ))
  })
  
  # output$plt1 <- flexdashboard::renderGauge({
  #   gauge(56, min = 0, max = 100, symbol = '%', gaugeSectors(
  #     success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")
  #   ))
  #   
  # })
  
  ##################################### Missing items summary View #####################################################
  output$tabmissingSummary = renderDataTable({
    Dt=data.frame(AccDB)
    missing.summary <- sapply(Dt, function(x) sum(is.na(x))) 
    indexs.missing <- sapply(Dt, function(x) sum(is.na(x))) > 0 
    num.variable.missing <- length(missing.summary[indexs.missing])
    
    freq.table.miss <- data.frame(Variable = names(missing.summary[indexs.missing]), Number.of.Missing = as.integer(missing.summary[indexs.missing]), 
                                   Percentage.of.Missing = paste0(round(as.numeric(prop.table(missing.summary[indexs.missing]))*100,2)," %") )
    
    freq.table.miss <- freq.table.miss %>% 
      select(Variable:Percentage.of.Missing) %>%
      arrange(desc(Number.of.Missing))
    
    datatable(freq.table.miss)
  })
  ##
  output$tabmissingSummaryDetails = renderDataTable({
    Dtd=reactdataAccountDB()
    Dtd=Dtd%>% 
      filter(is.na(Segment))
    #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
    #columns=c("Region","Load","Speed","cnt")
    datatable(Dtd,filter="bottom",class = 'cell-border stripe',rownames = FALSE)
  })
  
})
