library("shiny")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("date")
# library("tswge") #Error parsing manifest: Unable to determine package source for Bioconductor package waveslim: Repository must be specified


#read data
df_17_19 = read.csv("Bivin CC 2017-19_updated_headings.csv")
df_15_16 = read.csv("Bivin CC 2015-16_updated_headings.csv")
df_13_14 = read.csv("Bivin CC 2013-14_updated_headings.csv")

#data processing
df_17_19$date = date.mmddyy(mdy.date(match(substr(df_17_19$Month, 1, 3),month.abb),01,df_17_19$Year),sep="/")
df_15_16$date = date.mmddyy(mdy.date(match(substr(df_15_16$Month, 4, 6),month.abb),01,df_15_16$Year),sep="/")
df_13_14$date = date.mmddyy(mdy.date(match(substr(df_13_14$Month, 1, 3),month.abb),01,df_13_14$Year),sep="/")
df <- rbind(df_17_19,df_15_16,df_13_14)
invisible(df %>% filter(Account_Status != "Closed")) #remove inactive accounts
drops <- c("Metrics","Year","Month","House","Account_Status","Beverage_Type","Fiscal_Year","Premise","Customer_Street_Address","Customer_City",
           "Customer_Zip_Code","Longitude_Customer","Latitude_Customer","Customer","Vendor","Brand_ID","Brand","Size","Product_ID","Chain","Category",
           "Product_Type_ID","Qty_Per_Case","Alcohol_Proof","X9L_Cases","Dollar_Sales_Per_Case","Dollar_Sales_Per_9L_Case")
df = df[ , !(names(df) %in% drops)] #drop vars
df$Dollar_Sales = as.numeric(gsub('[$,]', '', df$Dollar_Sales)) #format $$$

# Create file with all possible combinations
combinations0 = as.data.frame(df %>% group_by(Product_Type, Product, Customer_ID) %>% tally(sort=TRUE))
combinations = combinations0 %>% filter(n >= 42)

cust_ID <- unique(combinations$Customer_ID)


ui <- fluidPage(
  titlePanel("Selection Tool Prototype"),
  sidebarPanel(
    h3("Make this adaptive?"), 
    helpText("Select store ID and product name to view standard cases purchased over time"), 
    selectInput(inputId = 'id_selection', label = 'Select Customer ID', choices = sort(cust_ID)),
    uiOutput("var1"),
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel('Primary Page',
        #verbatimTextOutput('text1'),
        #verbatimTextOutput('text2'),
        plotOutput("plot1"),
        textOutput('text3'), 
        #tableOutput("summaryTable")
        h3("Summary of Cases Sold"),
        verbatimTextOutput('text4')
      ), 
      #where column is x position, offset is y where zero is at the top?
      tabPanel('Secondary Page',
               fluidRow(
               column(6, plotOutput('p2plot1')),
               column(6, plotOutput('p2plot2')), 
               column(6, plotOutput('p2plot3')), 
               column(6, plotOutput('p2plot4'))
               )
      ), 
      tabPanel('Tertiary/Simple Forecast Page',
               sliderInput(inputId = "forecast", "Months to Forecast", min = 1, max = 12, value = 2),
               plotOutput('forecast1'),
               verbatimTextOutput(('ftext1')))
              
    )
    
  )
)

server <- function(input, output, session) {
  #if i put stuff here is it available to all subsections? 'not allowed without active reactive context' guess not
  
  
  outVar <- reactive({
    temp_store_df <- combinations[which(combinations$Customer_ID == input$id_selection), ]
    row_return <- sort(temp_store_df$Product) #populates a second input list with variables dependent on first input
  })
  
  output$var1 = renderUI({
    selectInput(inputId = 'p_select', label = 'Select Product', choices = outVar()) #order this alphabetically
  })
  
  output$plot1 <- renderPlot({
    y <- df[which(df$Customer_ID == input$id_selection), ]
    z <- y[which(y$Product == input$p_select), ]
    z$date <- as.Date(z$date, "%m/%d/%Y") #df comes in with dates in nonsequential order
    z <- z[order(z$date), ]
    ###DATE FILLING###
    full_date_sequence <- data.frame('date' = (seq(z$date[1], z$date[length(z$date)], by = 'month') ) ) %>% setDT()
    intake_data <- z %>% setDT() #datatable merge and replace na with zero
    full_date_frame <- intake_data[full_date_sequence, on = 'date'] 
    full_date_frame[is.na(full_date_frame)] <- 0
    z <- full_date_frame
    ###DATE FILLING ###
    
    #progress bar on replot
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    
    ggplot() +
      geom_line(data = z, aes(x = as.Date(date, '%m/%d/%Y'), y = STD_Cases)) +
      ggtitle(paste("Standard Case Sales of [", input$p_select, "]"), subtitle =  paste("For Customer [", input$id_selection, ']')) +
      xlab('Date') + ylab("Cases Purchased") + scale_y_continuous()
    
    
  })
  output$text3 <- renderText({
    y <- df[which(df$Customer_ID == input$id_selection), ]
    z <- y[which(y$Product == input$p_select), ]
    z$date <- as.Date(z$date, "%m/%d/%Y") 
    z <- z[order(z$date), ]
    ###DATE FILLING###
    full_date_sequence <- data.frame('date' = (seq(z$date[1], z$date[length(z$date)], by = 'month') ) ) %>% setDT()
    intake_data <- z %>% setDT() #datatable merge and replace na with zero
    full_date_frame <- intake_data[full_date_sequence, on = 'date'] 
    full_date_frame[is.na(full_date_frame)] <- 0
    z <- full_date_frame
    ###DATE FILLING ###
    
    nacounter <- which(z$STD_Cases == 0) %>% length() %>% sum() #count number of enties where there are zero cases ordered
    paste('[', nacounter, "] Months where no order for [", input$p_select, "] was placed")
  })
  output$text4 <- renderPrint({
    y <- df[which(df$Customer_ID == input$id_selection), ]
    z <- y[which(y$Product == input$p_select), ]
    z$date <- as.Date(z$date, "%m/%d/%Y") 
    z <- z[order(z$date), ]
    ###DATE FILLING###
    full_date_sequence <- data.frame('date' = (seq(z$date[1], z$date[length(z$date)], by = 'month') ) ) %>% setDT()
    intake_data <- z %>% setDT() #datatable merge and replace na with zero
    full_date_frame <- intake_data[full_date_sequence, on = 'date'] 
    full_date_frame[is.na(full_date_frame)] <- 0
    z <- full_date_frame
    ###DATE FILLING ###
    summary(z$STD_Cases)
  })
  #tab2 diagnostic plots
  output$p2plot1 <- renderPlot({
    y <- df[which(df$Customer_ID == input$id_selection), ]
    z <- y[which(y$Product == input$p_select), ]
    z$date <- as.Date(z$date, "%m/%d/%Y") 
    z <- z[order(z$date), ]
    ###DATE FILLING###
    full_date_sequence <- data.frame('date' = (seq(z$date[1], z$date[length(z$date)], by = 'month') ) ) %>% setDT()
    intake_data <- z %>% setDT() #datatable merge and replace na with zero
    full_date_frame <- intake_data[full_date_sequence, on = 'date'] 
    full_date_frame[is.na(full_date_frame)] <- 0
    z <- full_date_frame
    ###DATE FILLING ###
    
    #progress bar on replot
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      n <- 10
      for (i in 1:n) {dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        incProgress(1/n, detail = paste("Doing part", i))
        Sys.sleep(0.1)}   })
    #progress bar on replot ^^^^^
    
    plot(z$date, z$STD_Cases, type = 'l', main = z$Product[1])
  })
  output$p2plot2 <- renderPlot({
    y <- df[which(df$Customer_ID == input$id_selection), ]
    z <- y[which(y$Product == input$p_select), ]
    z$date <- as.Date(z$date, "%m/%d/%Y") 
    z <- z[order(z$date), ]
    ###DATE FILLING###
    full_date_sequence <- data.frame('date' = (seq(z$date[1], z$date[length(z$date)], by = 'month') ) ) %>% setDT()
    intake_data <- z %>% setDT() #datatable merge and replace na with zero
    full_date_frame <- intake_data[full_date_sequence, on = 'date'] 
    full_date_frame[is.na(full_date_frame)] <- 0
    z <- full_date_frame
    ###DATE FILLING ###
    acf(z$STD_Cases)
  })
  output$p2plot3 <- renderPlot({
    y <- df[which(df$Customer_ID == input$id_selection), ]
    z <- y[which(y$Product == input$p_select), ]
    z$date <- as.Date(z$date, "%m/%d/%Y") 
    z <- z[order(z$date), ]
    ###DATE FILLING###
    full_date_sequence <- data.frame('date' = (seq(z$date[1], z$date[length(z$date)], by = 'month') ) ) %>% setDT()
    intake_data <- z %>% setDT() #datatable merge and replace na with zero
    full_date_frame <- intake_data[full_date_sequence, on = 'date'] 
    full_date_frame[is.na(full_date_frame)] <- 0
    z <- full_date_frame
    ###DATE FILLING ###
    spectrum(z$STD_Cases, method = 'pgram', type = 'h')
  })
  output$p2plot4 <- renderPlot({
    y <- df[which(df$Customer_ID == input$id_selection), ]
    z <- y[which(y$Product == input$p_select), ]
    z$date <- as.Date(z$date, "%m/%d/%Y") 
    z <- z[order(z$date), ]
    ###DATE FILLING###
    full_date_sequence <- data.frame('date' = (seq(z$date[1], z$date[length(z$date)], by = 'month') ) ) %>% setDT()
    intake_data <- z %>% setDT() #datatable merge and replace na with zero
    full_date_frame <- intake_data[full_date_sequence, on = 'date'] 
    full_date_frame[is.na(full_date_frame)] <- 0
    z <- full_date_frame
    ###DATE FILLING ###
    spectrum(z$STD_Cases, method = 'ar')
  })
  #tab3
  
  #the forecast plot 
  output$forecast1 <- renderPlot({
    y <- df[which(df$Customer_ID == input$id_selection), ]
    z <- y[which(y$Product == input$p_select), ]
    z$date <- as.Date(z$date, "%m/%d/%Y") 
    z <- z[order(z$date), ]
    ###DATE FILLING###
    full_date_sequence <- data.frame('date' = (seq(z$date[1], z$date[length(z$date)], by = 'month') ) ) %>% setDT()
    intake_data <- z %>% setDT() #datatable merge and replace na with zero
    full_date_frame <- intake_data[full_date_sequence, on = 'date'] 
    full_date_frame[is.na(full_date_frame)] <- 0
    z <- full_date_frame
    ###DATE FILLING ###
    
    model_target <- z
    p_range = 0:5
    q_range = 0:5
    n_ahead = input$forecast #months
    
    pqc <- expand.grid(p_range, q_range)
    aic_holder <- vector()
    p_holder <- vector()
    q_holder <- vector()
    
     for(i in seq(1, length(pqc$Var1), 1)) {
       #print(pqc[i, ])
       temp_model <- arima(model_target$STD_Cases, order = c(pqc[i, 1], 0, pqc[i, 2]))
       #aic_holder <- append(aic_holder, temp_model$aic) #base AIC with k = 2?
       aic_holder <- append(aic_holder, AIC(temp_model, k = log(length(model_target$STD_Cases))))
       p_holder <- append(p_holder, pqc[i, 1])
       q_holder <- append(q_holder, pqc[i, 2])
     }
     
     aic_results <- data.frame(p = p_holder, q = q_holder, aic = aic_holder)
     aic_results <- aic_results[order(aic_results$aic), ]
     #aic_results
     
     m2 <- arima0(model_target$STD_Cases, order = c(aic_results$p[1], 0, aic_results$q[1]))
     #m2
     pred2 <- predict(m2, n_ahead)
     f2 <- append(model_target$STD_Cases, as.vector(pred2$pred))
     d_labs <- append(rep('data', length(model_target$STD_Cases)), rep('predictions', length(pred2$pred)))
     time_labs <- append(z$date, seq.Date(model_target$date[length(model_target$date)], by = 'month', length.out = (n_ahead+1))[-1])
     time_labs <- sort(time_labs)
     ep2 <- data.frame('cases' = f2, 'category' = d_labs, 'date' = time_labs)
    
     #progress bar on replot
     dat <- data.frame(x = numeric(0), y = numeric(0))
     withProgress(message = 'Making plot', value = 0, {
       # Number of times we'll go through the loop
       n <- 10
       
       for (i in 1:n) {
         # Each time through the loop, add another row of data. This is
         # a stand-in for a long-running computation.
         dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
         
         # Increment the progress bar, and update the detail text.
         incProgress(1/n, detail = paste("Doing part", i))
         
         # Pause for 0.1 seconds to simulate a long computation.
         Sys.sleep(0.1)
       }
     })
     
     ggplot(data = ep2) + 
       geom_point(aes(x = date, y = cases, color = category))+
       geom_line(aes(x = date, y = cases, color = category)) +
       ggtitle(paste('Customer ID: ', z$Customer_ID[1], '\n', z$Product[1]), subtitle = paste('ARIMA (', aic_results[1, 1], ',', 0, ',', aic_results[1, 2], ')')  )+
       scale_y_continuous(name = 'STD Cases Purchased')
     #arg/row mismatch - got it, watch pointers; y axis is kinda funky
  })
  output$ftext1 <- renderPrint({
    y <- df[which(df$Customer_ID == input$id_selection), ]
    z <- y[which(y$Product == input$p_select), ]
    z$date <- as.Date(z$date, "%m/%d/%Y") 
    z <- z[order(z$date), ]
    ###DATE FILLING###
    full_date_sequence <- data.frame('date' = (seq(z$date[1], z$date[length(z$date)], by = 'month') ) ) %>% setDT()
    intake_data <- z %>% setDT() #datatable merge and replace na with zero
    full_date_frame <- intake_data[full_date_sequence, on = 'date'] 
    full_date_frame[is.na(full_date_frame)] <- 0
    z <- full_date_frame
    ###DATE FILLING ###
    
    model_target <- z
    p_range = 0:5
    q_range = 0:5
    n_ahead = input$forecast #months
    
    pqc <- expand.grid(p_range, q_range)
    aic_holder <- vector()
    p_holder <- vector()
    q_holder <- vector()
    
    for(i in seq(1, length(pqc$Var1), 1)) {
      #print(pqc[i, ])
      temp_model <- arima(model_target$STD_Cases, order = c(pqc[i, 1], 0, pqc[i, 2]))
      #aic_holder <- append(aic_holder, temp_model$aic) #base AIC with k = 2?
      aic_holder <- append(aic_holder, AIC(temp_model, k = log(length(model_target$STD_Cases))))
      p_holder <- append(p_holder, pqc[i, 1])
      q_holder <- append(q_holder, pqc[i, 2])
    }
    
    aic_results <- data.frame(p = p_holder, q = q_holder, aic = aic_holder)
    aic_results <- aic_results[order(aic_results$aic), ]
    #aic_results
    
    m2 <- arima0(model_target$STD_Cases, order = c(aic_results$p[1], 0, aic_results$q[1]))
    #m2
    pred2 <- predict(m2, n_ahead)
    f2 <- append(model_target$STD_Cases, as.vector(pred2$pred))
    d_labs <- append(rep('data', length(model_target$STD_Cases)), rep('predictions', length(pred2$pred)))
    time_labs <- append(z$date, seq.Date(model_target$date[length(model_target$date)], by = 'month', length.out = (n_ahead+1))[-1])
    time_labs <- sort(time_labs)
    ep2 <- data.frame('cases' = f2, 'category' = d_labs, 'date' = time_labs)
    
    print(ep2[(length(ep2$cases)-n_ahead)+1:length(ep2$cases), ])
    #print(ep2)
  })
  #test if the scope of the function includes entire server section or do i have to run calcs for each input? 
  #look up a way to avoid having identical code blocks in separately updateing parts of the panel
  #other than making it a global functionnnnnnnnnn
  #server end
}

shinyApp(ui = ui, server = server)