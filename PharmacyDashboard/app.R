library(shiny)
library(shinythemes)
library(RODBC)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(plotly)
library(DT)
library(scales)

#Version 8.6

ui<-fluidPage( 
  tags$style(type="text/css",
             ".recalculating {opacity: .69;}"
  ),
  theme = shinytheme("flatly"),
  titlePanel(img(src="PharmacyLogo.png"), windowTitle = "Las Colinas Pharmacy"),
  navbarPage("Navigation :", selected = "Cap Lab", collapsible = TRUE,
             navbarMenu("Lab Inventory",
                        tabPanel("Cap Lab", #shinythemes::themeSelector(),
                                 fluidRow(column(12,
                                                 fluidRow(
                                                   column(2, style = "background-color:#D0D2D1;", HTML("<font size='3' color='firebrick'>Select which view you would like to see</font>"),
                                                          radioButtons("ui.show.cap", label = "Show Me:", choices = list("Top 20" = "Top20", "Top 30" = "Top30", "Only Negative" = "Only Negative", "Only Below 6 Day Par Level" = "Only Below Par" )),
                                                          sliderInput("ui.graph.cap", "Display -at most- only the following in stock quantities:", min = 0, max = 9000, value = 9000, step = 30)
                                                   ),
                                                   column(10, plotOutput("capPlot"))
                                                 ), 
                                                 fluidRow(
                                                   column(12, dataTableOutput("capTable"))
                                                 )
                                 )
                                 )),
                        tabPanel("Wet lab",
                                 sidebarLayout(
                                   sidebarPanel(HTML("<font size='3' color='firebrick'>Select which view you would like to see</font>"), width = 2,
                                                radioButtons("ui.show.wet", label = "Show Me:", choices = list("Top 20" = "Top20", "Top 30" = "Top30", "Only Negative" = "Only Negative", "Only Below 6 Day Par Level" = "Only Below Par"  )),
                                                sliderInput("ui.graph.wet", "Display -at most- only the following in stock quantities:", min = 0, max = 3000, value = 3000)
                                   ),
                                   mainPanel(width = 10,
                                             tabsetPanel(
                                               tabPanel("Inventory", plotOutput("wetPlot"), dataTableOutput("wetTable")),
                                               tabPanel("Batches", plotOutput("wetBatchPlot"), dataTableOutput("wetBatchTable"))
                                             ))
                                 )
                        ),
                        tabPanel("IV Lab", 
                                 fluidRow(column(12,
                                                 fluidRow(
                                                   column(2, style = "background-color:#D0D2D1;", HTML("<font size='3' color='firebrick'>Select which view you would like to see</font>"), 
                                                          radioButtons("ui.show.IV", label = "Show Me:", choices = list("Top 20" = "Top20", "Only Negative" = "Only Negative", "Only Below 6 Day Par Level" = "Only Below Par"  )),
                                                          sliderInput("ui.graph.IV", "Display -at most- only the following in stock quantities:", min = 0, max = 3000, value = 3000)
                                                   ),
                                                   column(10, plotOutput("ivPlot"))), 
                                                 fluidRow(
                                                   column(12, dataTableOutput("ivTable"))
                                                 )
                                 )
                                 )
                        )),
             navbarMenu("Vitamins",
                        tabPanel("Vitamin Inventory", 
                                 fluidRow(column(12,
                                                 fluidRow(
                                                   column(2, style = "background-color:#D0D2D1;",HTML("<font size='3' color='firebrick'>Select which view you would like to see</font>"), 
                                                          radioButtons("ui.show.vita", label = "Show Me:", choices = list("Top 20" = "Top20", "Top 30" = "Top30", 
                                                                                                                          "Only Negative" = "Only Negative", "Only Below Par Level" = "Only Below Par" ))
                                                   ),
                                                   column(10, plotOutput("vitaPlot"))), 
                                                 fluidRow(
                                                   column(12, dataTableOutput("vitaTable"))
                                                 )
                                 ))),
                        tabPanel("Vitamin Sales",
                                 fluidRow(column(12,
                                                 fluidRow(
                                                   column(2, style = "background-color:#D0D2D1;", HTML("<font size='3' color='firebrick'>Select which view you would like to see</font>"), 
                                                          radioButtons("ui.show.vita.sales", label = "Show Me:", choices = list("Top 20" = "Top20", "Top 30" = "Top30")),
                                                          dateRangeInput("ui.date", "Sales Date Range", start = Sys.Date() - 30)
                                                   ),
                                                   column(10, plotOutput("vitaSalesPlot"))), 
                                                 fluidRow(
                                                   column(12, dataTableOutput("vitaSalesTable"))
                                                 )
                                 ))
                        )),
             navbarMenu("Temp & Humidity Logs",
                        tabPanel("Labs",
                                 mainPanel(width = 12,
                                           tabsetPanel(
                                             tabPanel("Sterile Lab", plotlyOutput("iv.temp"), plotlyOutput("iv.humid"), dataTableOutput("table.temp.IV")),
                                             tabPanel("Sterile Lab Incubators", plotlyOutput("incubat.temp"), dataTableOutput("table.temp.incubat")),
                                             tabPanel("Non-Sterile Labs", plotlyOutput("NnSteril.temp"), plotlyOutput("NnSteril.humid"), dataTableOutput("table.temp.NnSteril"))
                                           ))
                        ),
                        tabPanel("Store",
                                 mainPanel(width = 12, plotlyOutput("store.temp"), plotlyOutput("store.humid"), dataTableOutput("table.temp.store"))
                        ),
                        tabPanel("Refrigerators & Freezers",
                                 mainPanel(width = 12,
                                           tabsetPanel(
                                             tabPanel("Refrigerators", plotlyOutput("fridge.temp"), dataTableOutput("table.temp.fridge")),
                                             tabPanel("Freezers", plotlyOutput("freezer.temp"), dataTableOutput("table.temp.freezer"))
                                           ))
                        )
             ),
             navbarMenu("Other",
                        tabPanel("Sodium Chloride Calculator - Intrathecals",
                                 fluidRow(
                                   column(12, "Sodium Chloride Calculator - Intrathecals", 
                                          fluidRow(
                                            column(2, wellPanel(
                                              h4("Enter the final volume of the compound:"),
                                              numericInput("finalVolume",
                                                           "mL",
                                                           min = 0, max = 100000, value = 42, step = 1)
                                            )),
                                            fluidRow(
                                              column(3, wellPanel(
                                                h4("Enter the strength of each ingredient (in mg/mL): "),
                                                numericInput("baclofen", "Baclofen:",
                                                             min = 0.1, max = 100, value = 0, step = 0.1),
                                                textOutput("baclofenG"), 
                                                numericInput("bupivacaine", "Bupivacaine:",
                                                             min = 0.001, max = 100, value = 0, step = 0.01),
                                                textOutput("bupivacaineG"), 
                                                numericInput("clonidine", "Clonidine:",
                                                             min = 0.1, max = 100, value = 0, step = 0.1),
                                                textOutput("clonidineG"), 
                                                numericInput("fentanyl", "Fentanyl Citrate:",
                                                             min = 0.1, max = 100, value = 0, step = 0.1),
                                                textOutput("fentanylG")
                                              )),
                                              column(3, wellPanel(
                                                h4("Enter the strength of each ingredient (in mg/mL): "),
                                                numericInput("hydromorphone", "Hydromorphone:",
                                                             min = 0.1, max = 100, value = 0, step = 0.1),
                                                textOutput("hydromorphoneG"), 
                                                numericInput("morphine", "Morphine:",
                                                             min = 0.1, max = 100, value = 0, step = 0.1),
                                                textOutput("morphineG"), 
                                                numericInput("other_API", "Other API:",
                                                             min = 0.1, max = 100, value = 0, step = 0.1),
                                                textOutput("otherG"),
                                                numericInput("other_E", "Other API's E-Value:",
                                                             min = 0.1, max = 100, value = 0, step = 0.1)
                                              )),
                                              fluidRow(
                                                column(3, wellPanel(
                                                  h4("The amount of sodium chloride needed to make this formulation isotonic: "),
                                                  textOutput("nacl")
                                                )))))
                                   ))),
                        tabPanel("Sterility Testing Cheat Sheet",
                                 fluidRow(column(12, uiOutput("USP71Frame"))
                                 ))
                        
             )
  ), 
  hr(),
  print("For use by Las Colinas Pharmacy. Version 8.6 **Beta** (2017).")
)

server <- function(input, output) {
  # SQL & DATA FRAMES
  {
    #************************RxQ************************
    date.today <- Sys.Date()
    date.today.yymmdd <- format(date.today, "%y%m%d")
    date.today.yyyymmdd <- format(date.today, "%Y%m%d")
    date.today.minus1Quarter <- date.today - 91.2
    date.today.minus1Quarter.yymmdd <- format(date.today.minus1Quarter, "%y%m%d")
    date.today.minus1Year <- date.today - 366
    date.today.minus1Year.yyyymmdd <- format(date.today.minus1Year, "%Y%m%d")
    
    #inital SQL and set table to only items that are inventoried
    db.rxq <- odbcDriverConnect('driver={SQL Server};server=;database=;uid=;pwd=')
    db.table <- as.data.frame(sqlQuery(db.rxq, paste("SELECT Name, Strength, NdcNumber, QtyInStock, ReorderPoint, LastDateDispensed, MetricName, rxqDrugInventory.IsValid, CustomField1, SUM(QuantityDispensed) AS SumQtyDispensed, SnoozeOrder, SnoozeUntil FROM rxqDrug INNER JOIN rxqDrugInventory ON rxqDrug.DrugId = rxqDrugInventory.DrugInventoryKey INNER JOIN rxqScriptTransaction ON rxqScriptTransaction.DrugId = rxqDrug.DrugId WHERE DateDispensedSQL BETWEEN  '",date.today.minus1Quarter,"' AND '",date.today,"' AND rxqDrugInventory.IsValid = '1' GROUP BY Name, Strength, NdcNumber, QtyInStock, ReorderPoint, LastDateDispensed, MetricName, rxqDrugInventory.IsValid, CustomField1, SnoozeOrder, SnoozeUntil")))
    #odbcClose(db.rxq)
    #db.table$LastDateDispensed <- parse_date_time(db.table$LastDateDispensed, orders = "mdy")
    #db.table$LastDateDispensed <- format(db.table$LastDateDispensed, "%m-%d-%Y")
    db.table$LastDateDispensed <- as.Date(db.table$LastDateDispensed)
    db.table$NdcNumber <- as.factor(db.table$NdcNumber)
    db.table <- mutate(db.table, SnoozeUntil2 = (ifelse(SnoozeOrder == 1, months(SnoozeUntil), "")))
    db.table$SnoozeUntil2 <- as.factor(db.table$SnoozeUntil2)
    
    # CAP LAB 
    db.table.caplab <- filter(db.table, CustomField1 == 'Cap Lab') %>% 
      mutate(ParLevel.30Day = (SumQtyDispensed / 91.2) * 30, ParLevel.6Day = (SumQtyDispensed / 91.2) * 6) 
    db.table.caplab$ParLevel.30Day <- ceiling(db.table.caplab$ParLevel.30Day)
    db.table.caplab$ParLevel.6Day <- ceiling(db.table.caplab$ParLevel.6Day)
    db.table.caplab.top20 <- head(arrange(db.table.caplab, desc(SumQtyDispensed)), n = 20)
    db.table.caplab.top30 <- head(arrange(db.table.caplab, desc(SumQtyDispensed)), n = 30)
    
    filterCapPlot20 <- reactive({
      db.table.caplab.top20 <- filter(db.table.caplab.top20, QtyInStock < input$ui.graph.cap)
    })
    
    filterCapPlot30 <- reactive({
      db.table.caplab.top30 <-  filter(db.table.caplab.top30, QtyInStock < input$ui.graph.cap)
    })
    
    # WET LAB
    db.table.wetlab <- filter(db.table, CustomField1 == 'Wet Lab') %>% 
      mutate(ParLevel.30Day = (SumQtyDispensed / 91.2) * 30, ParLevel.6Day = (SumQtyDispensed / 91.2) * 6)
    db.table.wetlab$ParLevel.30Day <- ceiling(db.table.wetlab$ParLevel.30Day)
    db.table.wetlab$ParLevel.6Day <- ceiling(db.table.wetlab$ParLevel.6Day)
    db.table.wetlab.top20 <- head(arrange(db.table.wetlab, desc(SumQtyDispensed)), n = 20)
    db.table.wetlab.top30 <- head(arrange(db.table.wetlab, desc(SumQtyDispensed)), n = 30)
    
    filterWetPlot20 <- reactive({
      db.table.wetlab.top20 <- filter(db.table.wetlab.top20, QtyInStock < input$ui.graph.wet)
    })
    
    filterWetPlot30 <- reactive ({ 
      db.table.wetlab.top30 <- filter(db.table.wetlab.top30, QtyInStock < input$ui.graph.wet)
    })
    
    # WET LAB BATCHES
    db.batches <- as.data.frame(sqlQuery(db.rxq, paste("SELECT Name, Strength, NdcNumber, rxqDrugInventory.QtyInStock AS QtyInStock, rxqDrugInventory.IsValid, CustomField1, 
                                                       rxqDrugBatch.QtyInStock AS BatchQtyInStock, rxqDrugBatch.IsValid AS BatchIsValid, LotNumber, ExpirationDate 
                                                       FROM rxqDrug INNER JOIN rxqDrugInventory ON rxqDrug.DrugId = rxqDrugInventory.DrugInventoryKey 
                                                       INNER JOIN rxqDrugBatch ON rxqDrug.DrugId = rxqDrugBatch.DrugId")))
    #odbcClose(db.rxq)
    db.table.wetlabBatch <- filter(db.batches, CustomField1 == 'Wet Lab' & IsValid == 0 & BatchIsValid == 1)
    db.table.wetlabBatch.agg <- aggregate(BatchQtyInStock ~ Name, data = db.table.wetlabBatch, sum)
    db.table.wetlabBatch$ExpirationDate <- parse_date_time(db.table.wetlabBatch$ExpirationDate, orders = "Ymd HMS")
    db.table.wetlabBatch$ExpirationDate <- format(db.table.wetlabBatch$ExpirationDate, "%m-%d-%Y")
    db.table.wetlabBatch$NdcNumber <- as.factor(db.table.wetlabBatch$NdcNumber)
    
    
    filterWetBatch <- reactive ({
      db.table.wetlabBatch.agg <- filter(db.table.wetlabBatch.agg, BatchQtyInStock < input$ui.graph.wet)
    })
    
    # IV LAB
    db.table.IVlab <- filter(db.table, CustomField1 == 'Sterile Lab') %>% 
      mutate(ParLevel.30Day = (SumQtyDispensed / 91.2) * 30, ParLevel.6Day = (SumQtyDispensed / 91.2) * 6)
    db.table.IVlab$ParLevel.30Day <- ceiling(db.table.IVlab$ParLevel.30Day)
    db.table.IVlab$ParLevel.6Day <- ceiling(db.table.IVlab$ParLevel.6Day)
    db.table.IVlab.top20 <- head(arrange(db.table.IVlab, desc(SumQtyDispensed)), n = 20)
    
    filterIVPlot20 <- reactive({
      db.table.IVlab.top20 <- filter(db.table.IVlab.top20, QtyInStock < input$ui.graph.IV)
    })
    
    #************************BzQ************************
    #inital SQL and set table to only items that are in the vitamin dept
    db.bzq <- odbcDriverConnect('driver={SQL Server};server=;database=;uid=;pwd=')
    bzq.table <- as.data.frame(sqlQuery(db.bzq, paste("SELECT DESCP AS Description, MFGR AS MFR, UPC, AVAIL AS QtyInStock, COST, STOCK, TRANS_DATE, ORDER_QTY, RETAIL, FULL_AMT, INITEM.NUMBER 
                                                      FROM INITEM INNER JOIN INSTOCK on INITEM.NUMBER = INSTOCK.NUMBER 
                                                      INNER JOIN OELINE on INITEM.NUMBER = OELINE.ITEM_NUM 
                                                      INNER JOIN OEREG on OELINE.INVOICE = OEREG.INVOICE 
                                                      WHERE INITEM.DEPT = 'Vit' AND TRANS_DATE BETWEEN '",date.today.minus1Year,"' AND '",date.today,"' AND INSTOCK.id != '820' 
                                                      AND (DESCP != 'ADULT TUSSIN 4oz HEALTH MART' AND DESCP != 'ultra protein bars' AND DESCP != 'COURIER') AND (STOCK = 'Y' OR STOCK = 'T')")))
    #odbcClose(db.bzq)
    bzq.table$TRANS_DATE <- as.POSIXct(bzq.table$TRANS_DATE, format = "%Y-%m-%d")
    
    #VITAMIN SALES FROM RxQ FOR VITAMIN INVENTORY
    db.rxq.otc <- as.data.frame(sqlQuery(db.rxq, paste("SELECT Name, NdcNumber, QuickLookup, Manufacturer, ContainerQuantity, SUM(QuantityDispensed) AS SumQtyDispensed 
                                                       FROM rxqDrug INNER JOIN rxqScriptTransaction ON rxqScriptTransaction.DrugId = rxqDrug.DrugId 
                                                       WHERE OverTheCounter = 'Y' AND DateDispensedSQL BETWEEN '",date.today.minus1Quarter,"' AND '",date.today,"' 
                                                       GROUP BY Name, NdcNumber, QuickLookup, Manufacturer, ContainerQuantity")))
    #odbcClose(db.rxq)
    db.rxq.otc <- dplyr::mutate(db.rxq.otc, SumQtyDispensed = SumQtyDispensed / ContainerQuantity)
    db.rxq.otc$SumQtyDispensed <- round(db.rxq.otc$SumQtyDispensed, 0)
    db.rxq.otc$NdcNumber <- as.factor(db.rxq.otc$NdcNumber)
    
    #VITAMIN SALES FROM RXQ FOR VITAMIN SALES
    db.rxq.otc.sales <- as.data.frame(sqlQuery(db.rxq, paste("SELECT Name, NdcNumber, QuickLookup, Manufacturer, DateDispensedSQL, ContainerQuantity, SUM(QuantityDispensed) AS SumQtyDispensed 
                                                             FROM rxqDrug INNER JOIN rxqScriptTransaction ON rxqScriptTransaction.DrugId = rxqDrug.DrugId 
                                                             WHERE CustomField3 = 'YES' AND DateDispensedSQL BETWEEN '",date.today.minus1Year,"' AND '",date.today,"' 
                                                             GROUP BY Name, NdcNumber, QuickLookup, Manufacturer, ContainerQuantity, DateDispensedSQL")))
    #odbcClose(db.rxq)
    db.rxq.otc.sales <- dplyr::mutate(db.rxq.otc.sales, SumQtyDispensed = SumQtyDispensed / ContainerQuantity)
    db.rxq.otc.sales$SumQtyDispensed <- as.integer(db.rxq.otc.sales$SumQtyDispensed)
    db.rxq.otc.sales$NdcNumber <- as.factor(db.rxq.otc.sales$NdcNumber)
    db.rxq.otc.sales$DateDispensedSQL <- as.Date(db.rxq.otc.sales$DateDispensedSQL) 
    
    #VITAMIN INVENTORY
    db.table.vita <- bzq.table
    db.table.vita.b <- filter(db.table.vita, TRANS_DATE >= date.today.minus1Quarter & TRANS_DATE <= date.today)
    db.table.vita.r <- filter(db.rxq.otc.sales, DateDispensedSQL >= date.today.minus1Quarter & DateDispensedSQL <= date.today)
    db.table.vita.b.agg <- aggregate(ORDER_QTY ~ Description + UPC + MFR + QtyInStock + NUMBER, data = db.table.vita.b, sum)
    db.table.vita.r.agg <- aggregate(SumQtyDispensed ~ Name + NdcNumber , data = db.table.vita.r, sum)
    db.table.vita.c <- left_join(db.table.vita.b.agg, db.table.vita.r.agg, by = c("UPC" = "NdcNumber"))
    db.table.vita.c <- mutate(db.table.vita.c, SumQtyDispensed = ifelse(is.na(SumQtyDispensed),0,SumQtyDispensed), CombinedSales = ORDER_QTY + SumQtyDispensed, ParLevel = (CombinedSales / 91.2) * 60)
    db.table.vita.c$ParLevel <- as.integer(round(floor(db.table.vita.c$ParLevel), 0))
    db.table.vita.c$QtyInStock <- as.integer(db.table.vita.c$QtyInStock)
    db.table.vita.c.top20 <- head(arrange(db.table.vita.c, desc(ORDER_QTY)), n = 20)
    db.table.vita.c.top30 <- head(arrange(db.table.vita.c, desc(ORDER_QTY)), n = 30)
    
    filterVitaSalesPlot20 <- reactive({
      db.table.vita.sales.bzq <- filter(bzq.table, TRANS_DATE >= input$ui.date[1] & TRANS_DATE <= input$ui.date[2])
      db.table.vita.sales.rxq <- filter(db.rxq.otc.sales, DateDispensedSQL >= input$ui.date[1] & DateDispensedSQL <= input$ui.date[2])
      db.table.vita.sales.bzq.agg <- aggregate(ORDER_QTY ~ Description + UPC , data = db.table.vita.sales.bzq, sum)
      db.table.vita.sales.rxq.agg <- aggregate(SumQtyDispensed ~ Name + NdcNumber , data = db.table.vita.sales.rxq, sum)
      db.table.vita.sales.top20 <- left_join(db.table.vita.sales.bzq.agg, db.table.vita.sales.rxq.agg, by = c("UPC" = "NdcNumber"))
      db.table.vita.sales.top20 <- mutate(db.table.vita.sales.top20, SumQtyDispensed = ifelse(is.na(SumQtyDispensed),0,SumQtyDispensed), CombinedSales = ORDER_QTY + SumQtyDispensed)
      db.table.vita.sales.top20 <- head(arrange(db.table.vita.sales.top20, desc(CombinedSales)), n = 20)
    })
    
    filterVitaSalesPlot30 <- reactive({
      db.table.vita.sales.bzq <- filter(bzq.table, TRANS_DATE >= input$ui.date[1] & TRANS_DATE <= input$ui.date[2])
      db.table.vita.sales.rxq <- filter(db.rxq.otc.sales, DateDispensedSQL >= input$ui.date[1] & DateDispensedSQL <= input$ui.date[2])
      db.table.vita.sales.bzq.agg <- aggregate(ORDER_QTY ~ Description + UPC , data = db.table.vita.sales.bzq, sum)
      db.table.vita.sales.rxq.agg <- aggregate(SumQtyDispensed ~ Name + NdcNumber , data = db.table.vita.sales.rxq, sum)
      db.table.vita.sales.top30 <- left_join(db.table.vita.sales.bzq.agg, db.table.vita.sales.rxq.agg, by = c("UPC" = "NdcNumber"))
      db.table.vita.sales.top30 <- mutate(db.table.vita.sales.top30, SumQtyDispensed = ifelse(is.na(SumQtyDispensed),0,SumQtyDispensed), CombinedSales = ORDER_QTY + SumQtyDispensed)
      db.table.vita.sales.top30 <- head(arrange(db.table.vita.sales.top30, desc(CombinedSales)), n = 30)
    })
    
    #FOR BZQ AND RXQ COMBINED SALES DATA TABLE
    filterVitaSalesTable <- reactive({
      db.table.vita.sales.bzq <- filter(bzq.table, TRANS_DATE >= input$ui.date[1] & TRANS_DATE <= input$ui.date[2])
      db.table.vita.sales.rxq <- filter(db.rxq.otc.sales, DateDispensedSQL >= input$ui.date[1] & DateDispensedSQL <= input$ui.date[2])
      db.table.vita.sales.bzq.agg <- aggregate(cbind(ORDER_QTY, FULL_AMT) ~ Description + MFR + UPC + COST + RETAIL, data = db.table.vita.sales.bzq, sum)
      db.table.vita.sales.rxq.agg <- aggregate(SumQtyDispensed ~ Name + NdcNumber , data = db.table.vita.sales.rxq, sum)
      db.table.vita.sales <- left_join(db.table.vita.sales.bzq.agg, db.table.vita.sales.rxq.agg, by = c("UPC" = "NdcNumber"))
      db.table.vita.sales <-  mutate(db.table.vita.sales, SumQtyDispensed = ifelse(is.na(SumQtyDispensed),0,SumQtyDispensed), CombinedSales = ORDER_QTY + SumQtyDispensed, TotalItemSales = FULL_AMT + (SumQtyDispensed * RETAIL)) %>% 
        select(Description, UPC, ORDER_QTY, SumQtyDispensed, CombinedSales, COST, RETAIL, TotalItemSales) %>%
        filter(CombinedSales > 0)
      db.table.vita.sales$UPC <- as.factor(db.table.vita.sales$UPC)
      db.table.vita.sales$ORDER_QTY <- as.integer(db.table.vita.sales$ORDER_QTY)
      db.table.vita.sales$SumQtyDispensed <- as.integer(db.table.vita.sales$SumQtyDispensed)
      db.table.vita.sales$CombinedSales <- as.integer(db.table.vita.sales$CombinedSales)
      db.table.vita.sales$ORDER_QTY <- as.integer(db.table.vita.sales$ORDER_QTY)
      db.table.vita.sales$COST <- as.double(round(db.table.vita.sales$COST, 2))
      db.table.vita.sales$RETAIL <- as.double(round(db.table.vita.sales$RETAIL, 2))
      db.table.vita.sales$TotalItemSales <- as.double(round(db.table.vita.sales$TotalItemSales, 2))
      return(db.table.vita.sales)
      
    })
    
  }
  
  # ************************NaCl Calculator************************
  {
    
    output$baclofenG <- reactive({
      (input$baclofen / 1000) * input$finalVolume
    })
    
    baclofenW <- reactive({
      (((input$baclofen / 1000) * input$finalVolume) * (0.234)) / 0.009
    })
    
    output$bupivacaineG <- reactive({
      (input$bupivacaine / 1000) * input$finalVolume
    })
    
    bupivacaineW <- reactive({
      (((input$bupivacaine / 1000) * input$finalVolume) * (0.170)) / 0.009
    })
    
    output$clonidineG <- reactive({
      (input$clonidine / 1000) * input$finalVolume
    })
    
    clonidineW <- reactive({
      (((input$clonidine / 1000) * input$finalVolume) * (0.22)) / 0.009
    })
    
    output$fentanylG <- reactive({
      (input$fentanyl / 1000) * input$finalVolume
    })
    
    fentanylW <- reactive({
      (((input$fentanyl / 1000) * input$finalVolume) * (0.111)) / 0.009
    })
    
    output$hydromorphoneG <- reactive({
      (input$hydromorphone / 1000) * input$finalVolume
    })
    
    hydromorphoneW <- reactive({
      if(input$hydromorphone > 0){
        hydromorphoneW <- ifelse(is.nan(((-0.048*log((input$hydromorphone / 10))) + 0.2238)) == TRUE, 0, ((-0.048*log((input$hydromorphone / 10))) + 0.2238))
        hydromorphoneW <- (((input$hydromorphone / 1000) * input$finalVolume) * hydromorphoneW) / 0.009
      }
      else{
        hydromorphoneW <- 0
      }
    })
    
    output$morphineG <- reactive({
      (input$morphine / 1000) * input$finalVolume
    })
    
    morphineW <- reactive({
      if(input$morphine >0){
        morphineW <- ifelse(is.nan(((-0.03*log((input$morphine / 10))) + 0.1401)) == TRUE, 0, ((-0.03*log((input$morphine / 10))) + 0.1401))
        morphineW <- (((input$morphine / 1000) * input$finalVolume) * morphineW) / 0.009
      }
      else{
        morphineW <- 0
      }
    })
    
    output$otherG <- reactive({
      (input$other_API / 1000) * input$finalVolume
    })
    
    otherW <- reactive({
      (((input$other_API / 1000) * input$finalVolume) * (input$other_E)) / 0.009
    })
    
    water <- reactive({
      sum(baclofenW() , bupivacaineW() , clonidineW() , fentanylW() , hydromorphoneW() , morphineW() , otherW())
    })
    
    nacl <- reactive({
      nacl <- (input$finalVolume - water()) * 0.009
      nacl <- round(nacl, 4)
      nacl <- ifelse(nacl > 0, nacl, 0)
    })
    
    output$nacl <- renderText({nacl()})
    
  }
  
  #************************Temperature************************
  {
    date.today.minus72H <- (date.today - 3)
    
    temp.IV.data <- read.csv("F:\\RTRTemperatureLogs\\CurrentLogs\\IV Lab.csv")
    temp.NnSteril.data <- read.csv("F:\\RTRTemperatureLogs\\CurrentLogs\\NnSteril.csv")
    temp.Fridge.data <- read.csv("F:\\RTRTemperatureLogs\\CurrentLogs\\Fridge.csv")
    temp.NL.data <- read.csv("F:\\RTRTemperatureLogs\\CurrentLogs\\Non-Lab.csv")
    temp.Incubat.data <- read.csv("F:\\RTRTemperatureLogs\\CurrentLogs\\Incubat.csv")
    temp.Freezer.data <- read.csv("F:\\RTRTemperatureLogs\\CurrentLogs\\Freezer.csv")
    
    temp.IV.data2 <- rename(temp.IV.data, "Ante Room Temp (F)" = Ante, "Ante Room Humidity (%)" = X, "Lab Temp (F)" = Lab, "Lab Humidity (%)" = X.1, "Buffer Room Temp (F)" = Buffer, "Buffer Room Humidity (%)" = X.2)
    temp.IV.data2 <- temp.IV.data2[-1,-8]
    temp.IV.data2 <- mutate(temp.IV.data2, "Ante Temp Dec" = `Ante Room Temp (F)`, "Lab Temp Dec" = `Lab Temp (F)`, "Buffer Temp Dec" = `Buffer Room Temp (F)`, Date = DateTime, Time = DateTime)
    temp.IV.data2$DateTime <- as.POSIXct(temp.IV.data2$DateTime, format="%m/%d/%Y %H:%M:%S")
    temp.IV.data2$Time <- format(as.POSIXct(strptime(temp.IV.data2$Time,"%m/%d/%Y %H:%M:%S",tz="")), format = "%r")
    temp.IV.data2$Date <- format(as.POSIXct(strptime(temp.IV.data2$Date,"%m/%d/%Y %H:%M:%S",tz="")), format = "%m/%d/%Y")
    temp.IV.data2$`Ante Room Temp (F)` <- as.numeric(as.character(temp.IV.data2$`Ante Room Temp (F)`)) %>%
      round(digits = 0)
    temp.IV.data2$`Ante Room Humidity (%)` <- as.numeric(as.character(temp.IV.data2$`Ante Room Humidity (%)`)) 
    temp.IV.data2$`Lab Temp (F)` <- as.numeric(as.character(temp.IV.data2$`Lab Temp (F)`)) %>%
      round(digits = 0)
    temp.IV.data2$`Lab Humidity (%)` <- as.numeric(as.character(temp.IV.data2$`Lab Humidity (%)`))
    temp.IV.data2$`Buffer Room Temp (F)` <- as.numeric(as.character(temp.IV.data2$`Buffer Room Temp (F)`)) %>%
      round(digits = 0)
    temp.IV.data2$`Buffer Room Humidity (%)` <- as.numeric(as.character(temp.IV.data2$`Buffer Room Humidity (%)`))
    
    temp.NnSteril.data2 <- rename(temp.NnSteril.data, "Wet Lab Temp (F)" = Wet.Lab, "Wet Lab Humidity (%)" = X, "Cap Lab Temp (F)" = Cap.Lab, "Cap Lab Humidity (%)" = X.1)
    temp.NnSteril.data2 <- temp.NnSteril.data2[-1,-6]
    temp.NnSteril.data2 <- mutate(temp.NnSteril.data2, "Wet Temp Dec" = `Wet Lab Temp (F)`, "Cap Temp Dec" = `Cap Lab Temp (F)`, Date = DateTime, Time = DateTime)
    temp.NnSteril.data2$DateTime <- as.POSIXct(temp.NnSteril.data2$DateTime, format="%m/%d/%Y %H:%M:%S")
    temp.NnSteril.data2$Time <- format(as.POSIXct(strptime(temp.NnSteril.data2$Time,"%m/%d/%Y %H:%M:%S",tz="")), format = "%r")
    temp.NnSteril.data2$Date <- format(as.POSIXct(strptime(temp.NnSteril.data2$Date,"%m/%d/%Y %H:%M:%S",tz="")), format = "%m/%d/%Y")
    temp.NnSteril.data2$`Wet Lab Temp (F)` <- as.numeric(as.character(temp.NnSteril.data2$`Wet Lab Temp (F)`)) %>%
      round(digits = 0)
    temp.NnSteril.data2$`Wet Lab Humidity (%)` <- as.numeric(as.character(temp.NnSteril.data2$`Wet Lab Humidity (%)`))
    temp.NnSteril.data2$`Cap Lab Temp (F)` <- as.numeric(as.character(temp.NnSteril.data2$`Cap Lab Temp (F)`)) %>%
      round(digits = 0)
    temp.NnSteril.data2$`Cap Lab Humidity (%)` <- as.numeric(as.character(temp.NnSteril.data2$`Cap Lab Humidity (%)`))
    
    temp.NL.data2 <- rename(temp.NL.data, "Store Room Temp (F)" = Store.Rm, "Store Room Humidity (%)" = X, "Drug Shelves Temp (F)" = Drugs, "Drug Shelves Humidity (%)" = X.1, "Server Room Temp (F)" = ServerRm)
    temp.NL.data2 <- temp.NL.data2[-1,-7]
    temp.NL.data2 <- mutate(temp.NL.data2, "Store Room Temp Dec" = `Store Room Temp (F)`, "Drug Shelves Temp Dec" = `Drug Shelves Temp (F)`, "Server Room Temp Dec" = `Server Room Temp (F)`, Date = DateTime, Time = DateTime)
    temp.NL.data2$DateTime <- as.POSIXct(temp.NL.data2$DateTime, format="%m/%d/%Y %H:%M:%S")
    temp.NL.data2$Time <- format(as.POSIXct(strptime(temp.NL.data2$Time,"%m/%d/%Y %H:%M:%S",tz="")), format = "%r")
    temp.NL.data2$Date <- format(as.POSIXct(strptime(temp.NL.data2$Date,"%m/%d/%Y %H:%M:%S",tz="")), format = "%m/%d/%Y")
    temp.NL.data2$`Store Room Temp (F)` <- as.numeric(as.character(temp.NL.data2$`Store Room Temp (F)`)) %>%
      round(digits = 0)
    temp.NL.data2$`Store Room Humidity (%)` <- as.numeric(as.character(temp.NL.data2$`Store Room Humidity (%)`))
    temp.NL.data2$`Drug Shelves Temp (F)` <- as.numeric(as.character(temp.NL.data2$`Drug Shelves Temp (F)`)) %>%
      round(digits = 0)
    temp.NL.data2$`Drug Shelves Humidity (%)` <- as.numeric(as.character(temp.NL.data2$`Drug Shelves Humidity (%)`))
    temp.NL.data2$`Server Room Temp (F)` <- as.numeric(as.character(temp.NL.data2$`Server Room Temp (F)`)) %>%
      round(digits = 0)
    
    temp.Incubat.data2 <- rename(temp.Incubat.data, "Incubator 'A' Temp (F)" = A, "Incubator 'B' Temp (F)" = B, "Incubator 'C' Temp (F)" = C)
    temp.Incubat.data2 <- temp.Incubat.data2[-1,-5]
    temp.Incubat.data2 <- mutate(temp.Incubat.data2, "Incubator 'A' Temp Dec" = `Incubator 'A' Temp (F)`, "Incubator 'B' Temp Dec" = `Incubator 'B' Temp (F)`, "Incubator 'C' Temp Dec" = `Incubator 'C' Temp (F)`, Date = DateTime, Time = DateTime)
    temp.Incubat.data2$DateTime <- as.POSIXct(temp.Incubat.data2$DateTime, format="%m/%d/%Y %H:%M:%S")
    temp.Incubat.data2$Time <- format(as.POSIXct(strptime(temp.Incubat.data2$Time,"%m/%d/%Y %H:%M:%S",tz="")), format = "%r")
    temp.Incubat.data2$Date <- format(as.POSIXct(strptime(temp.Incubat.data2$Date,"%m/%d/%Y %H:%M:%S",tz="")), format = "%m/%d/%Y")
    temp.Incubat.data2$`Incubator 'A' Temp (F)` <- as.numeric(as.character(temp.Incubat.data2$`Incubator 'A' Temp (F)`)) %>%
      round(digits = 0)
    temp.Incubat.data2$`Incubator 'B' Temp (F)` <- as.numeric(as.character(temp.Incubat.data2$`Incubator 'B' Temp (F)`)) %>%
      round(digits = 0)
    temp.Incubat.data2$`Incubator 'C' Temp (F)` <- as.numeric(as.character(temp.Incubat.data2$`Incubator 'C' Temp (F)`)) %>%
      round(digits = 0)
    
    temp.Fridge.data2 <- rename(temp.Fridge.data, "Front Rx Fridge Temp (F)" = FrontRx, "Wet Lab Fridge Temp (F)" = Wet.Lab, "Sterile Lab Fridge Temp (F)" = IV.Lab)
    temp.Fridge.data2 <- temp.Fridge.data2[-1,-5]
    temp.Fridge.data2 <- mutate(temp.Fridge.data2, "Front Rx Fridge Temp Dec" = `Front Rx Fridge Temp (F)`, "Wet Lab Fridge Temp Dec" = `Wet Lab Fridge Temp (F)`, "Sterile Lab Fridge Temp Dec" = `Sterile Lab Fridge Temp (F)`, Date = DateTime, Time = DateTime)
    temp.Fridge.data2$DateTime <- as.POSIXct(temp.Fridge.data2$DateTime, format="%m/%d/%Y %H:%M:%S")
    temp.Fridge.data2$Time <- format(as.POSIXct(strptime(temp.Fridge.data2$Time,"%m/%d/%Y %H:%M:%S",tz="")), format = "%r")
    temp.Fridge.data2$Date <- format(as.POSIXct(strptime(temp.Fridge.data2$Date,"%m/%d/%Y %H:%M:%S",tz="")), format = "%m/%d/%Y")
    temp.Fridge.data2$`Front Rx Fridge Temp (F)` <- as.numeric(as.character(temp.Fridge.data2$`Front Rx Fridge Temp (F)`)) %>%
      round(digits = 0)
    temp.Fridge.data2$`Wet Lab Fridge Temp (F)` <- as.numeric(as.character(temp.Fridge.data2$`Wet Lab Fridge Temp (F)`)) %>%
      round(digits = 0)
    temp.Fridge.data2$`Sterile Lab Fridge Temp (F)` <- as.numeric(as.character(temp.Fridge.data2$`Sterile Lab Fridge Temp (F)`)) %>%
      round(digits = 0)
    
    temp.Freezer.data2 <- rename(temp.Freezer.data, "Wet Lab Freezer Temp (F)" = Wet.Lab, "Store Room Freezer Temp (F)" = Store.Rm)
    temp.Freezer.data2 <- temp.Freezer.data2[-1,-4]
    temp.Freezer.data2 <- mutate(temp.Freezer.data2, "Wet Lab Freezer Temp Dec" = `Wet Lab Freezer Temp (F)`, "Store Room Freezer Temp Dec" = `Store Room Freezer Temp (F)`, Date = DateTime, Time = DateTime)
    temp.Freezer.data2$DateTime <- as.POSIXct(temp.Freezer.data2$DateTime, format="%m/%d/%Y %H:%M:%S")
    temp.Freezer.data2$Time <- format(as.POSIXct(strptime(temp.Freezer.data2$Time,"%m/%d/%Y %H:%M:%S",tz="")), format = "%r")
    temp.Freezer.data2$Date <- format(as.POSIXct(strptime(temp.Freezer.data2$Date,"%m/%d/%Y %H:%M:%S",tz="")), format = "%m/%d/%Y")
    temp.Freezer.data2$`Wet Lab Freezer Temp (F)` <- as.numeric(as.character(temp.Freezer.data2$`Wet Lab Freezer Temp (F)`)) %>%
      round(digits = 0)
    temp.Freezer.data2$`Store Room Freezer Temp (F)` <- as.numeric(as.character(temp.Freezer.data2$`Store Room Freezer Temp (F)`)) %>%
      round(digits = 0)
    
  }
  
  # PLOTS & TABLES
  {
    myTheme1 <- theme(panel.background = element_rect(fill = 'lightgrey', color = 'lightgrey'), 
                      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
                      panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
                      legend.position = "none")
    
    # CAP INVENTORY PLOT & TABLE  
    {
      output$capPlot <-  renderPlot({
        
        if(input$ui.show.cap == "Top20"){
          
          myplot.p <- ggplot(filterCapPlot20(), aes(y = QtyInStock, x = reorder(Name, -QtyInStock), 
                                                    color = QtyInStock > 0, fill = QtyInStock > 0)) 
          
          nudgeMax <- max(pmax(filterCapPlot20()$QtyInStock, filterCapPlot20()$ParLevel.30Day))
          nudger <- nudgeMax * .05
          validate(need(nrow(filterCapPlot20()) > 0, 'There is no data to display'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Drug", title = "Top 20 Capsule Inventory") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "firebrick")) +
            geom_errorbar(aes(y = ParLevel.30Day, x = Name, ymin = QtyInStock, ymax = ParLevel.30Day), color="darkred", width=0.5) +
            geom_text(aes(x = Name, y = nudgeMax + nudger, label = QtyInStock))  +
            myTheme1
          
        }
        
        else if(input$ui.show.cap == "Top30"){
          
          myplot.p <- ggplot(filterCapPlot30(), aes(y = QtyInStock, x = reorder(Name, -QtyInStock), 
                                                    color = QtyInStock > 0, fill = QtyInStock > 0)) 
          
          nudgeMax <- max(pmax(filterCapPlot30()$QtyInStock, filterCapPlot30()$ParLevel.30Day))
          nudger <- nudgeMax * .05
          validate(need(nrow(filterCapPlot30()) > 0, 'There is no data to display'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Drug", title = "Top 30 Capsule Inventory") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "firebrick")) +
            geom_errorbar(aes(y = ParLevel.30Day, x = Name, ymin = QtyInStock, ymax = ParLevel.30Day), color="darkred", width=0.5) +
            geom_text(aes(x = Name, y = nudgeMax + nudger, label = QtyInStock))  +
            myTheme1
        }
        
        else if(input$ui.show.cap == "Only Negative"){
          db.table.caplab <- filter(db.table.caplab, db.table.caplab$QtyInStock < 0)
          myplot.p <- ggplot(db.table.caplab, aes(y = QtyInStock, x = reorder(Name, -QtyInStock), 
                                                  color = QtyInStock > 0, fill = QtyInStock > 0)) 
          
          nudgeMin <- min(db.table.caplab$QtyInStock)
          nudger <- nudgeMin * .05
          validate(need(nrow(db.table.caplab) > 0, 'There are no negative quantities. Great job gang! :)'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Drug", title = "Capsule Inventory: only items with negative stock quantities") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "firebrick")) +
            geom_text(aes(x = Name, y =  nudgeMin - nudger, label = QtyInStock))  +
            myTheme1 
        }
        
        else if(input$ui.show.cap == "Only Below Par"){
          db.table.caplab <- filter(db.table.caplab, db.table.caplab$QtyInStock < db.table.caplab$ParLevel.6Day & db.table.caplab$SnoozeOrder == 0 )%>%
            head(arrange(desc(QtyInStock)),n = 30)
          myplot.p <- ggplot(db.table.caplab, aes(y = QtyInStock, x = reorder(Name, -QtyInStock), 
                                                  color = QtyInStock > 0, fill = QtyInStock > 0))
          
          nudgeMax <- max(pmax(db.table.caplab$QtyInStock, db.table.caplab$ParLevel.6Day))
          nudger <- nudgeMax * .05
          validate(need(nrow(db.table.caplab) > 0, 'There are no quantities below the 6 day par level. Great job gang! :)'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Drug", title = "Capsule Inventory: only items below 6 day par level") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "firebrick")) +
            geom_errorbar(aes(y = ParLevel.6Day, x = Name, ymin = QtyInStock, ymax = ParLevel.6Day), color="darkred", width=0.5) +
            geom_text(aes(x = Name, y = nudgeMax + nudger, label = QtyInStock))  +
            myTheme1
          
        }
      })
      
      db.table.caplab.filter <- select(db.table.caplab, Name, Strength, NdcNumber, ParLevel.6Day, ParLevel.30Day, QtyInStock, LastDateDispensed, SnoozeUntil2) %>%
        rename("6 Day Par Level" = ParLevel.6Day, "30 day Par Level" = ParLevel.30Day, "Don't make until" = SnoozeUntil2)
      output$capTable <- renderDataTable(arrange(db.table.caplab.filter, Name),extensions = 'Buttons', options = list(
        dom = 'Blfrtip', buttons = list('print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')), lengthMenu = c(10, 25, 50, -1)), filter = 'top')
    }
    
    # WET INVENTORY PLOT & TABLE 
    {
      output$wetPlot <-  renderPlot({
        
        if(input$ui.show.wet == "Top20"){
          
          myplot.p <- ggplot(filterWetPlot20(), aes(y = QtyInStock, x = reorder(Name, -QtyInStock), 
                                                    color = QtyInStock > 0, fill = QtyInStock > 0))
          
          nudgeMax <- max(pmax(filterWetPlot20()$QtyInStock, filterWetPlot20()$ParLevel.30Day))
          nudger <- nudgeMax * .05
          validate(need(nrow(filterWetPlot20()) > 0, 'There is no data to display'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Drug", title = "Top 20 Wet Lab Inventory") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightgoldenrod1", "FALSE" = "firebrick")) +
            geom_errorbar(aes(y = ParLevel.30Day, x = Name, ymin = QtyInStock, ymax = ParLevel.30Day), color="darkred", width=0.5) +
            geom_text(aes(x = Name, y = nudgeMax + nudger, label = QtyInStock))  +
            myTheme1
        }
        
        else if(input$ui.show.wet == "Top30"){
          
          myplot.p <- ggplot(filterWetPlot30(), aes(y = QtyInStock, x = reorder(Name, -QtyInStock), 
                                                    color = QtyInStock > 0, fill = QtyInStock > 0))
          
          nudgeMax <- max(pmax(filterWetPlot30()$QtyInStock, filterWetPlot30()$ParLevel.30Day))
          nudger <- nudgeMax * .05
          validate(need(nrow(filterWetPlot30()) > 0, 'There is no data to display'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Drug", title = "Top 30 Wet Lab Inventory") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightgoldenrod1", "FALSE" = "firebrick")) +
            geom_errorbar(aes(y = ParLevel.30Day, x = Name, ymin = QtyInStock, ymax = ParLevel.30Day), color="darkred", width=0.5) +
            geom_text(aes(x = Name, y = nudgeMax + nudger, label = QtyInStock))  +
            myTheme1
        }
        
        else if(input$ui.show.wet == "Only Negative"){
          
          db.table.wetlab <- filter(db.table.wetlab, db.table.wetlab$QtyInStock < 0)
          myplot.p <- ggplot(db.table.wetlab, aes(y = QtyInStock, x = reorder(Name, -QtyInStock), 
                                                  color = QtyInStock > 0, fill = QtyInStock > 0))
          
          nudgeMin <- min(db.table.wetlab$QtyInStock)
          nudger <- nudgeMin * .05
          validate(need(nrow(db.table.wetlab) > 0, 'There are no negative quantities. Great job gang! :)'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Drug", title = "Wet Lab Inventory: only items with negative stock quantities") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightgoldenrod1", "FALSE" = "firebrick")) +
            geom_text(aes(x = Name, y = nudgeMin - nudger, label = QtyInStock))  +
            myTheme1 
        }
        
        else if(input$ui.show.wet == "Only Below Par"){
          db.table.wetlab <- filter(db.table.wetlab, db.table.wetlab$QtyInStock < db.table.wetlab$ParLevel.6Day & db.table.wetlab$SnoozeOrder == 0 )%>%
            head(arrange(desc(QtyInStock)),n = 30)
          myplot.p <- ggplot(db.table.wetlab, aes(y = db.table.wetlab$QtyInStock, x = reorder(db.table.wetlab$Name, -QtyInStock), 
                                                  color = db.table.wetlab$QtyInStock > 0, fill = QtyInStock > 0))
          
          nudgeMax <- max(pmax(db.table.wetlab$QtyInStock, db.table.wetlab$ParLevel.6Day))
          nudger <- nudgeMax * .05
          validate(need(nrow(db.table.wetlab) > 0, 'There are no quantities below par level. Great job gang! :)'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Drug", title = "Wet Lab Inventory: only items below par level") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightgoldenrod1", "FALSE" = "firebrick")) +
            geom_errorbar(aes(y = ParLevel.6Day, x = Name, ymin = QtyInStock, ymax = ParLevel.6Day), color="darkred", width=0.5) +
            geom_text(aes(x = Name, y = nudgeMax + nudger, label = QtyInStock))  +
            myTheme1
        }
      })
      db.table.wetlab.filter <- select(db.table.wetlab, Name, Strength, NdcNumber, ParLevel.6Day, ParLevel.30Day, QtyInStock, LastDateDispensed, SnoozeUntil2)%>%
        rename("6 Day Par Level" = ParLevel.6Day, "30 day Par Level" = ParLevel.30Day, "Don't make until" = SnoozeUntil2)
      output$wetTable <- renderDataTable(arrange(db.table.wetlab.filter, Name), extensions = 'Buttons', options = list(
        dom = 'Blfrtip', buttons = list('print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')), lengthMenu = c(10, 25, 50, -1)), filter = 'top')
      
    }
    
    # WET BATCHES PLOT & TABLE
    {
      output$wetBatchPlot <-  renderPlot({
        myplot.p <- ggplot(filterWetBatch(), aes(y = BatchQtyInStock, x = reorder(Name, -BatchQtyInStock), 
                                                 color = BatchQtyInStock > 0, fill = BatchQtyInStock > 500))
        
        validate(need(nrow(filterWetBatch()) > 0, 'There is no data to display'))
        nudgeMax <- max(filterWetBatch()$BatchQtyInStock)
        nudger <- nudgeMax * .05
        
        myplot.p + geom_bar(stat = 'identity') +
          coord_flip() +
          labs(y = "Qty in Stock", x = "Drug", title = "Wet Lab Batch Inventory (Approx.)") +
          scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
          scale_fill_manual(values = c("TRUE" = "green4", "FALSE" = "yellow")) +
          geom_text(aes(x = Name, y = nudgeMax + nudger, label = BatchQtyInStock))  +
          myTheme1
      })
      db.table.wetlabBatch.filter <- select(db.table.wetlabBatch, Name, Strength, NdcNumber, BatchQtyInStock, LotNumber, ExpirationDate)
      output$wetBatchTable <- renderDataTable(arrange(db.table.wetlabBatch.filter, Name), extensions = 'Buttons', options = list(
        dom = 'Blfrtip', buttons = list('print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')), lengthMenu = c(10, 25, 50, -1)), filter = 'top')
    }
    
    # IV INVENTORY PLOT & TABLE
    {
      output$ivPlot <-  renderPlot({
        
        if(input$ui.show.IV == "Top20"){
          myplot.p <- ggplot(filterIVPlot20(), aes(y = QtyInStock, x = reorder(Name, -QtyInStock), 
                                                   color = QtyInStock > 0, fill = QtyInStock > 0))
          
          nudgeMax <- max(pmax(filterIVPlot20()$QtyInStock, filterIVPlot20()$ParLevel.30Day))
          nudger <- nudgeMax * .05
          validate(need(nrow(filterIVPlot20()) > 0, 'There is no data to display'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Drug", title = "Top 20 Sterile Lab Inventory") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightgreen", "FALSE" = "firebrick")) +
            geom_errorbar(aes(y = ParLevel.30Day, x = Name, ymin = QtyInStock, ymax = ParLevel.30Day), color="darkred", width=0.5) +
            geom_text(aes(x = Name, y = nudgeMax + nudger, label = QtyInStock))  +
            myTheme1
        }
        
        else if(input$ui.show.IV == "Only Negative"){
          db.table.IVlab <- filter(db.table.IVlab, db.table.IVlab$QtyInStock < 0)
          myplot.p <- ggplot(db.table.IVlab, aes(y = db.table.IVlab$QtyInStock, x = reorder(db.table.IVlab$Name, -QtyInStock), 
                                                 color = db.table.IVlab$QtyInStock > 0, fill = QtyInStock > 0))
          
          nudgeMin <- min(db.table.IVlab$QtyInStock)
          nudger <- nudgeMin * .05
          validate(need(nrow(db.table.IVlab) > 0, 'There are no negative quantities. Great job gang! :)'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Drug", title = "Sterile Lab Inventory: only items with negative stock quantities") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightgreen", "FALSE" = "firebrick")) +
            geom_text(aes(x = Name, y = nudgeMin - nudger, label = QtyInStock))  +
            myTheme1 
        }
        
        else if(input$ui.show.IV == "Only Below Par"){
          db.table.IVlab <- filter(db.table.IVlab, db.table.IVlab$QtyInStock < db.table.IVlab$ParLevel.6Day & db.table.IVlab$SnoozeOrder == 0 )
          myplot.p <- ggplot(db.table.IVlab, aes(y = db.table.IVlab$QtyInStock, x = reorder(db.table.IVlab$Name, -QtyInStock), 
                                                 color = db.table.IVlab$QtyInStock > 0, fill = QtyInStock > 0))
          
          nudgeMax <- max(pmax(db.table.IVlab$QtyInStock, db.table.IVlab$ParLevel.6Day))
          nudger <- nudgeMax * .05
          validate(need(nrow(db.table.IVlab) > 0, 'There are no quantities below par level. Great job gang! :)'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Drug", title = "Sterile Lab Inventory: only items below par level") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightgreen", "FALSE" = "firebrick")) +
            geom_errorbar(aes(y = ParLevel.6Day, x = Name, ymin = QtyInStock, ymax = ParLevel.6Day), color="darkred", width=0.5) +
            geom_text(aes(x = Name, y = nudgeMax + nudger, label = QtyInStock))  +
            myTheme1
        }
        
      })
      
      db.table.IVlab.filter <- select(db.table.IVlab, Name, Strength, NdcNumber, ParLevel.6Day, ParLevel.30Day, QtyInStock, LastDateDispensed, SnoozeUntil2)%>%
        rename("6 Day Par Level" = ParLevel.6Day, "30 day Par Level" = ParLevel.30Day, "Don't make until" = SnoozeUntil2)
      output$ivTable <- renderDataTable(arrange(db.table.IVlab.filter, Name), extensions = 'Buttons', options = list(
        dom = 'Blfrtip', buttons = list('print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')), lengthMenu = c(10, 25, 50, -1)), filter = 'top')
    }
    
    # VITAMIN INVENTORY PLOT & TABLE
    {
      output$vitaPlot <-  renderPlot({
        
        if(input$ui.show.vita == "Top20" ){
          myplot.p <- ggplot(db.table.vita.c.top20, aes(y = QtyInStock, x = reorder(Description, -QtyInStock), 
                                                        color = QtyInStock > 0, fill = QtyInStock > 0)) 
          
          nudgeMax <- max(pmax(db.table.vita.c.top20$QtyInStock, db.table.vita.c.top20$ParLevel))
          nudger <- nudgeMax * .05
          validate(need(nrow(db.table.vita.c.top20) > 0, 'There is no data to display'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Vitamin", title = "Top 20 Vitamin Inventory") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "coral", "FALSE" = "firebrick")) +
            geom_errorbar(aes(y = ParLevel, x = Description, ymin = QtyInStock, ymax = ParLevel), color="darkred", width=0.5) +
            geom_text(aes(x = Description, y = nudgeMax + nudger, label = QtyInStock))  +
            myTheme1
          
        }
        
        else if(input$ui.show.vita == "Top30"){
          myplot.p <- ggplot(db.table.vita.c.top30, aes(y = QtyInStock, x = reorder(Description, -QtyInStock), 
                                                        color = QtyInStock > 0, fill = QtyInStock > 0)) 
          
          nudgeMax <- max(pmax(db.table.vita.c.top30$QtyInStock, db.table.vita.c.top30$ParLevel))
          nudger <- nudgeMax * .05
          validate(need(nrow(db.table.vita.c.top30) > 0, 'There is no data to display'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Vitamin", title = "Top 30 Vitamin Inventory") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "coral", "FALSE" = "firebrick")) +
            geom_errorbar(aes(y = ParLevel, x = Description, ymin = QtyInStock, ymax = ParLevel), color="darkred", width=0.5) +
            geom_text(aes(x = Description, y = nudgeMax + nudger, label = QtyInStock))  +
            myTheme1
        }
        
        else if(input$ui.show.vita == "Only Negative"){
          db.table.vita.c <- filter(db.table.vita.c, db.table.vita.c$QtyInStock < 0)
          
          myplot.p <- ggplot(db.table.vita.c, aes(y = QtyInStock, x = reorder(Description, -QtyInStock), 
                                                  color = QtyInStock > 0, fill = QtyInStock > 0)) 
          
          nudgeMin <- min(db.table.vita.c$QtyInStock)
          nudger <- nudgeMin * .05
          validate(need(nrow(db.table.vita.c) > 0, 'There are no negative quantities. Great job gang! :)'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Vitamin", title = "Vitamin Inventory") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "firebrick")) +
            geom_text(aes(x = Description, y =  nudgeMin - nudger, label = QtyInStock))  +
            myTheme1 
          
        }
        
        else if(input$ui.show.vita == "Only Below Par"){
          db.table.vita.c <- filter(db.table.vita.c, db.table.vita.c$QtyInStock < db.table.vita.c$ParLevel)
          
          myplot.p <- ggplot(db.table.vita.c, aes(y = QtyInStock, x = reorder(Description, -QtyInStock), 
                                                  color = QtyInStock > 0, fill = QtyInStock > 0))
          
          nudgeMax <- max(pmax(db.table.vita.c$QtyInStock, db.table.vita.c$ParLevel))
          nudger <- nudgeMax * .05
          validate(need(nrow(db.table.vita.c) > 0, 'There are no quantities below the reorder point. Great job gang! :)'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty in Stock", x = "Vitamin", title = "Vitamin Inventory: Only items below reorder point") +
            scale_color_manual(guide = FALSE, values = setNames(c('black','black'),c(T, F))) +
            scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "firebrick")) +
            geom_errorbar(aes(y = ParLevel, x = Description, ymin = QtyInStock, ymax = ParLevel), color="darkred", width=0.5) +
            geom_text(aes(x = Description, y = nudgeMax + nudger, label = QtyInStock))  +
            myTheme1
          
        }
        
      })
      
      db.table.vita.c.filter <- select(db.table.vita.c, Description, MFR, UPC, NUMBER, QtyInStock, ParLevel)%>%
        mutate(QuantitytoOrder = as.integer(ifelse(ParLevel - QtyInStock > 0, ParLevel - QtyInStock, 0))) %>%
        rename("60 Day Par Level" = ParLevel, "Quantity to Order" = QuantitytoOrder)
      
      output$vitaTable <- renderDataTable(db.table.vita.c.filter, extensions = 'Buttons', options = list(
        dom = 'Blfrtip', buttons = list('print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')), lengthMenu = c(10, 25, 50, -1)), filter = 'top')
      
    }
    
    # VITAMIN SALES PLOT & TABLE
    {
      output$vitaSalesPlot <-  renderPlot({
        
        if(input$ui.show.vita.sales == "Top20"){
          
          myplot.p <- ggplot(filterVitaSalesPlot20(), aes(y = CombinedSales, x = reorder(Description, CombinedSales), fill = 'coral')) 
          
          nudgeMax <- max(filterVitaSalesPlot20()$CombinedSales)
          nudger <- nudgeMax * .05
          validate(need(nrow(filterVitaSalesPlot20()) > 0, 'There is no data to display'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty Sold", x = "Name", title = "Vitamin Sales") +
            geom_text(aes(x = Description, y = nudgeMax + nudger, label = CombinedSales)) +
            myTheme1
          
        }
        
        else if(input$ui.show.vita.sales == "Top30"){
          
          myplot.p <- ggplot(filterVitaSalesPlot30(), aes(y = CombinedSales, x = reorder(Description, CombinedSales), fill = 'coral')) 
          
          nudgeMax <- max(filterVitaSalesPlot30()$CombinedSales)
          nudger <- nudgeMax * .05
          validate(need(nrow(filterVitaSalesPlot30()) > 0, 'There is no data to display'))
          
          myplot.p + geom_bar(stat = 'identity') +
            coord_flip() +
            labs(y = "Qty Sold", x = "Name", title = "Top 30 Vitamin Sales") +
            geom_text(aes(x = Description, y = nudgeMax + nudger, label = CombinedSales)) +
            myTheme1
          
        }
      })
      
      output$vitaSalesTable <- renderDataTable(filterVitaSalesTable(), colnames = c('BzQ Sales (#)' = 'ORDER_QTY', 'RxQ Sales (#)' = 'SumQtyDispensed', 'Total Sales (#)' = 'CombinedSales', 'Item Cost ($)' = 'COST', 'Retail Price ($)' = 'RETAIL', 'Total Item Sales ($)' = 'TotalItemSales'), extensions = 'Buttons', options = list(
        dom = 'Blfrtip', buttons = list('print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')), lengthMenu = c(10, 25, 50, -1)), filter = 'top')
      
    }
    
    # TEMP PLOTS & TABLES
    {
      ## Temp Plots
      output$iv.temp <- renderPlotly({
        temp.IV.data3 <- filter(temp.IV.data2, DateTime > date.today.minus72H)
        iv.temp <- ggplot(na.omit(temp.IV.data3), aes(x = DateTime))
        
        iv.temp + geom_line(aes(y = `Buffer Room Temp (F)`, group = 1), color = 'green4') +
          geom_line(aes(y = `Ante Room Temp (F)`, group = 1), color = 'magenta') +
          geom_line(aes(y = `Lab Temp (F)`, group = 1), color = 'tomato') +
          geom_point(aes(y = `Buffer Room Temp (F)`), color = 'green4') +
          geom_point(aes(y = `Ante Room Temp (F)`), color = 'magenta') +
          geom_point(aes(y = `Lab Temp (F)`), color = 'tomato') +
          labs(title="Sterile Lab Temperature", y = "Temperature (F)", x = "Date & Time")
        ggplotly()
      })
      
      output$iv.humid <- renderPlotly({
        temp.IV.data3 <- filter(temp.IV.data2, DateTime > date.today.minus72H)
        iv.humid <- ggplot(na.omit(temp.IV.data3), aes(x = DateTime))
        
        iv.humid + geom_line(aes(y = `Buffer Room Humidity (%)`, group = 1), color = 'green4') +
          geom_line(aes(y = `Ante Room Humidity (%)`, group = 1), color = 'magenta') +
          geom_line(aes(y = `Lab Humidity (%)`, group = 1), color = 'tomato') +
          geom_point(aes(y = `Buffer Room Humidity (%)`), color = 'green4') +
          geom_point(aes(y = `Ante Room Humidity (%)`), color = 'magenta') +
          geom_point(aes(y = `Lab Humidity (%)`), color = 'tomato') +
          labs(title="Sterile Lab Humidity", y = "Humidity (%)", x = "Date & Time") 
        ggplotly()
      })
      
      output$NnSteril.temp <- renderPlotly({
        temp.NnSteril.data3 <- filter(temp.NnSteril.data2, DateTime > date.today.minus72H)
        NnSteril.temp <- ggplot(na.omit(temp.NnSteril.data3), aes(x = DateTime))
        
        NnSteril.temp + geom_line(aes(y = `Wet Lab Temp (F)`, group = 1), color = 'olivedrab4') +
          geom_line(aes(y = `Cap Lab Temp (F)`, group = 1), color = 'brown') +
          geom_point(aes(y = `Wet Lab Temp (F)`), color = 'olivedrab4') +
          geom_point(aes(y = `Cap Lab Temp (F)`), color = 'brown') +
          labs(title="Non-Sterile Labs Temperature", y = "Temperature (F)", x = "Date & Time") 
        ggplotly()
      })
      
      output$NnSteril.humid <- renderPlotly({
        temp.NnSteril.data3 <- filter(temp.NnSteril.data2, DateTime > date.today.minus72H)
        NnSteril.humid <- ggplot(na.omit(temp.NnSteril.data3), aes(x = DateTime))
        
        NnSteril.humid + geom_line(aes(y = `Wet Lab Humidity (%)`, group = 1), color = 'olivedrab4') +
          geom_point(aes(y = `Wet Lab Humidity (%)`), color = 'olivedrab4') +
          geom_line(aes(y = `Cap Lab Humidity (%)`, group = 1), color = 'brown') +
          geom_point(aes(y = `Cap Lab Humidity (%)`), color = 'brown') +
          labs(title="Non-Sterile Labs Humidity", y = "Humidity (%)", x = "Date & Time") 
        ggplotly()
      })
      
      output$store.temp <- renderPlotly({
        temp.NL.data3 <- filter(temp.NL.data2, DateTime > date.today.minus72H)
        store.temp <- ggplot(na.omit(temp.NL.data3), aes(x = DateTime))
        
        store.temp + geom_line(aes(y = `Store Room Temp (F)`, group = 1), color = 'steelblue3') +
          geom_line(aes(y = `Drug Shelves Temp (F)`, group = 1), color = 'springgreen4') +
          geom_line(aes(y = `Server Room Temp (F)`, group = 1), color = 'orangered') +
          geom_point(aes(y = `Store Room Temp (F)`), color = 'steelblue3') +
          geom_point(aes(y = `Drug Shelves Temp (F)`), color = 'springgreen4') +
          geom_point(aes(y = `Server Room Temp (F)`), color = 'orangered') +
          labs(title="Store Temperatures", y = "Temperature (F)", x = "Date & Time") 
        ggplotly()
      })
      
      output$store.humid <- renderPlotly({
        temp.NL.data3 <- filter(temp.NL.data2, DateTime > date.today.minus72H)
        store.humid <- ggplot(na.omit(temp.NL.data3), aes(x = DateTime))
        
        store.humid + geom_line(aes(y = `Store Room Humidity (%)`, group = 1), color = 'steelblue3') +
          geom_line(aes(y = `Drug Shelves Humidity (%)`, group = 1), color = 'springgreen4') +
          geom_point(aes(y = `Store Room Humidity (%)`), color = 'steelblue3') +
          geom_point(aes(y = `Drug Shelves Humidity (%)`), color = 'springgreen4') +
          labs(title="Store Humidity", y = "Humidity (%)", x = "Date & Time") 
        ggplotly()
      })
      
      output$incubat.temp <- renderPlotly({
        temp.Incubat.data3 <- filter(temp.Incubat.data2, DateTime > date.today.minus72H)
        incubat.temp <- ggplot(na.omit(temp.Incubat.data3), aes(x = DateTime))
        
        incubat.temp + geom_line(aes(y = `Incubator 'A' Temp (F)`, group = 1), color = 'green4') +
          geom_point(aes(y = `Incubator 'A' Temp (F)`), color = 'green4') +
          geom_line(aes(y = `Incubator 'B' Temp (F)`, group = 1), color = 'tomato') +
          geom_point(aes(y = `Incubator 'B' Temp (F)`), color = 'tomato') +
          geom_line(aes(y = `Incubator 'C' Temp (F)`, group = 1), color = 'peru') +
          geom_point(aes(y = `Incubator 'C' Temp (F)`), color = 'peru') +
          labs(title="Incubator Temperatures", y = "Temperature (F)", x = "Date & Time") 
        ggplotly()
      })
      
      output$fridge.temp <- renderPlotly({
        temp.Fridge.data3 <- filter(temp.Fridge.data2, DateTime > date.today.minus72H)
        fridge.temp <- ggplot(na.omit(temp.Fridge.data3), aes(x = DateTime))
        
        fridge.temp + geom_line(aes(y = `Front Rx Fridge Temp (F)`, group = 1), color = 'blue') +
          geom_point(aes(y = `Front Rx Fridge Temp (F)`), color = 'blue') +
          geom_line(aes(y = `Wet Lab Fridge Temp (F)`, group = 1), color = 'deepskyblue3') +
          geom_point(aes(y = `Wet Lab Fridge Temp (F)`), color = 'deepskyblue3') +
          geom_line(aes(y = `Sterile Lab Fridge Temp (F)`, group = 1), color = 'royalblue3') +
          geom_point(aes(y = `Sterile Lab Fridge Temp (F)`), color = 'royalblue3') +
          labs(title="Refrigerator Temperatures", y = "Temperature (F)", x = "Date & Time") 
        ggplotly()
      })
      
      output$freezer.temp <- renderPlotly({
        temp.Freezer.data3 <- filter(temp.Freezer.data2, DateTime > date.today.minus72H)
        freezer.temp <- ggplot(na.omit(temp.Freezer.data3), aes(x = DateTime))
        
        freezer.temp + geom_line(aes(y = `Wet Lab Freezer Temp (F)`, group = 1), color = 'blue') +
          geom_point(aes(y = `Wet Lab Freezer Temp (F)`), color = 'blue') +
          geom_line(aes(y = `Store Room Freezer Temp (F)`, group = 1), color = 'deepskyblue3') +
          geom_point(aes(y = `Store Room Freezer Temp (F)`), color = 'deepskyblue3') +
          labs(title="Freezer Temperatures", y = "Temperature (F)", x = "Date & Time")
        ggplotly()
      })
      
      
      ## Data Tables
      output$table.temp.IV <- DT::renderDataTable(select(temp.IV.data2, Date, Time, `Buffer Temp Dec`, `Buffer Room Humidity (%)`, `Ante Temp Dec`, `Ante Room Humidity (%)`, `Lab Temp Dec`, `Lab Humidity (%)`), colnames = c("Buffer Room Temp (F)" = "Buffer Temp Dec", "Ante Room Temp (F)" = "Ante Temp Dec", "Lab Temp (F)" = "Lab Temp Dec"), extensions = 'Buttons', options = list(
        dom = 'Blfrtip', buttons = list('print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')), lengthMenu = c(10, 25, 50, -1)), filter = 'top')
      
      output$table.temp.NnSteril <- DT::renderDataTable(select(temp.NnSteril.data2, Date, Time, `Wet Temp Dec`, `Wet Lab Humidity (%)`, `Cap Temp Dec`, `Cap Lab Humidity (%)`), colnames = c("Wet Lab Temp (F)" = "Wet Temp Dec", "Cap Lab Temp (F)" = "Cap Temp Dec"), extensions = 'Buttons', options = list(
        dom = 'Blfrtip', buttons = list('print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')), lengthMenu = c(10, 25, 50, -1)), filter = 'top')
      
      output$table.temp.incubat <- DT::renderDataTable(select(temp.Incubat.data2, Date, Time, `Incubator 'A' Temp Dec`, `Incubator 'B' Temp Dec`, `Incubator 'C' Temp Dec`), colnames = c("Incubator 'A' Temp (F)" = "Incubator 'A' Temp Dec", "Incubator 'B' Temp (F)" = "Incubator 'B' Temp Dec", "Incubator 'C' Temp (F)" = "Incubator 'C' Temp Dec"), extensions = 'Buttons', options = list(
        dom = 'Blfrtip', buttons = list('print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')), lengthMenu = c(10, 25, 50, -1)), filter = 'top')
      
      output$table.temp.store <- DT::renderDataTable(select(temp.NL.data2, Date, Time, `Store Room Temp Dec`, `Store Room Humidity (%)`, `Drug Shelves Temp Dec`, `Drug Shelves Humidity (%)`, `Server Room Temp Dec`), colnames = c("Store Room Temp (F)" = "Store Room Temp Dec", "Drug Shelves Temp (F)" = "Drug Shelves Temp Dec", "Server Room Temp (F)" = "Server Room Temp Dec"), extensions = 'Buttons', options = list(
        dom = 'Blfrtip', buttons = list('print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')), lengthMenu = c(10, 25, 50, -1)), filter = 'top')
      
      output$table.temp.fridge <- DT::renderDataTable(select(temp.Fridge.data2, Date, Time, `Front Rx Fridge Temp Dec`, `Wet Lab Fridge Temp Dec`, `Sterile Lab Fridge Temp Dec`), colnames = c("Front Rx Fridge Temp (F)" = "Front Rx Fridge Temp Dec", "Wet Lab Fridge Temp (F)" = "Wet Lab Fridge Temp Dec", "Sterile Lab Fridge Temp (F)" = "Sterile Lab Fridge Temp Dec"), extensions = 'Buttons', options = list(
        dom = 'Blfrtip', buttons = list('print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')), lengthMenu = c(10, 25, 50, -1)), filter = 'top')
      
      output$table.temp.freezer <- renderDataTable(select(temp.Freezer.data2, Date, Time, `Wet Lab Freezer Temp Dec`, `Store Room Freezer Temp Dec`), colnames = c("Wet Lab Freezer Temp (F)" = "Wet Lab Freezer Temp Dec", "Store Room Freezer Temp (F)" = "Store Room Freezer Temp Dec"), extensions = 'Buttons', options = list(
        dom = 'Blfrtip', buttons = list('print', list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')), lengthMenu = c(10, 25, 50, -1)), filter = 'top')
      
    }
  }
  
  # USP <71> STERILITY TESTING CHEAT SHEET
  {
    url2 <- paste0("https://docs.google.com/spreadsheets/d/1o-GV8iAFq62OCtOKVRCefZMPNkFgU1LEzte0G38rR9M/edit#gid=551671504")
    
    output$USP71Frame <- renderUI({
      usp_71 <- tags$iframe(src=url2, height=600, width=1500)
      usp_71
    })
    
  }
  
}
shinyApp(ui= ui, server = server)


