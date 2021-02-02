#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("ggplot2")
library("tidyverse")
library("dplyr")
library("date")
library('ranger')
library('randomForest')
library('caret')
library(shiny)
library(data.table)
library(nnfor)
#source('script_storage2.R') #source not updating with changes?
##### FUNCTION LIST #####
#Equal Means
#accept df object and return forcasts 1:12
em_ts <- function(n_ahead, target_df){
  return(rep((mean(target_df$STD_Cases)),  n_ahead))}
#bootleg AIC5 with no tswge
#target_vector <- target_df$STD_Cases 
aic_boot2 <- function(target_vector){
  pqc <- expand.grid(0:5, 0:2)
  aic_holder <- vector()
  p_holder <- vector()
  q_holder <- vector()
  for(i in 1:length(pqc$Var1)) {
    temp_model <- arima(target_vector, order = c(pqc[i, 1], 0, pqc[i, 2]), method = 'ML')
    #aic_holder <- append(aic_holder, temp_model$aic) #base AIC with k = 2?
    aic_holder[i] <- AIC(temp_model, k = log(length(target_vector)))
    p_holder <- append(p_holder, pqc[i, 1])
    q_holder <- append(q_holder, pqc[i, 2])}
  aic_results <- data.frame(p = p_holder, q = q_holder, aic = aic_holder)
  aic_results <- aic_results[order(aic_results$aic), ]
  return(head(aic_results, 1))}
ar_ts_boot <- function(n_ahead, target_df){
  aic_temp <- aic_boot2(target_df$STD_Cases)
  if (aic_temp$p == 0){
    newphi = 1}
  else{newphi = aic_temp$p}
  #model_temp <- arima0(target_df$STD_Cases, order = c(newphi, 0, 0), method = 'ML') #note q is zero for AR model
  #model_temp <- arima(target_df$STD_Cases, order = c(newphi, 0, 0), method = 'ML')
  pred_temp <- predict(arima(target_df$STD_Cases, order = c(newphi, 0, 0), method = 'ML'), n_ahead)
  return(pred_temp$pred)}
arma_ts_boot <- function(n_ahead, target_df){
  aic_temp <- aic_boot2(target_df$STD_Cases)
  if (aic_temp$p == 0){
    newphi = 1}
  else newphi = aic_temp$p #error if inside curly brackets??? 
  #model_temp <- arima(target_df$STD_Cases, order = c(aic_temp$p, 0, newphi)) #note q is not zero for ARMA model
  pred_temp <- predict(arima(target_df$STD_Cases, order = c(aic_temp$p, 0, newphi), method = 'ML'), n_ahead) 
  return(pred_temp$pred)}
ari_ts_boot <- function(n_ahead, target_df){
  diff1 <- diff(target_df$STD_Cases)
  aic_temp <- aic_boot2(diff1)
  if (aic_temp$p == 0){
    newphi = 1}
  else{newphi = aic_temp$p}
  pred_temp <- predict(arima(target_df$STD_Cases, order = c(newphi, 1, 0), method = 'ML'), n.ahead = n_ahead)
  return(pred_temp$pred)}
arima_ts_boot <- function(n_ahead, target_df){
  diff1 = diff(target_df$STD_Cases)
  aic_temp <- aic_boot2(diff1)
  if (aic_temp$p == 0){
    newphi = 1}
  else{newphi = aic_temp$p}
  pred_temp <- predict(arima(target_df$STD_Cases, order = c(newphi, 1, aic_temp$q), method = 'ML'), n.ahead = n_ahead)
  return(pred_temp$pred)}
ari_s12_ts_boot <- function(n_ahead, target_df){
  diff1 <- diff(target_df$STD_Cases)
  aic_temp <- aic_boot2(diff1)
  if (aic_temp$p == 0){
    newphi = 1
  } else {
    newphi = aic_temp$p
  } 
  pred_temp <- predict(arima(target_df$STD_Cases, order = c(newphi, 0, 0), seasonal = c(0, 0, 12), method = 'ML'), n.ahead = 12)
  return(pred_temp$pred) }
arima_s12_ts_boot <- function(n_ahead, target_df){
  diff1 <- diff(target_df$STD_Cases)
  aic_temp <- aic_boot2(diff1)
  if (aic_temp$p == 0){
    newphi = 1
  } else {
    newphi = aic_temp$p
  } 
  pred_temp <- predict(arima(target_df$STD_Cases, order = c(newphi, 0, aic_temp$q), seasonal = c(0, 0, 12), method = 'ML'), n.ahead = 12)
  return(pred_temp$pred) }
#RF function
library(ranger)
library(randomForest)
library(caret)
#with no graph
rf_ts <- function(n_ahead, target_df){
  target_df$date <- as.Date(target_df$date, '%m/%d/%Y')
  tlen = length(target_df$STD_Cases)
  sample_train <- target_df[1:(tlen-n_ahead), ]
  sample_test <- target_df[(tlen-n_ahead +1):tlen, ]
  sample_test$Dollar_Sales[is.na(sample_test$Dollar_Sales)] <- 0
  rf1 <- randomForest(STD_Cases ~date + Dollar_Sales, data = sample_train, na.action = na.exclude)
  pred1 = predict(rf1, newdata = sample_test)
  rf_ase <- mean((sample_test$STD_Cases - pred1)^2)
  predictions <- data.frame(date = sample_test$date, forecast = pred1)
  results = c(predictions, data.frame(ASE = rf_ase))
  #return(results)
  return(results$forecast)}
nnc <- function(target_df, n_ahead, reps, hd_vector, display){
  #date_line <- as.Date(target_df$date, '%m/%d/%Y') #add date back in after it works
  tlen <- length(target_df$STD_Cases)
  sample_train <- target_df[1:(tlen-n_ahead), ]
  sample_test <- target_df[(tlen-n_ahead +1):tlen, ]
  
  ts_train<- as.ts(sample_train$STD_Cases)
  fit_mlp <- mlp(ts_train, reps = reps, hd = hd_vector)
  
  mlp_pred <- forecast(fit_mlp, h = n_ahead)
  mlp_ase <- mean((sample_test$STD_Cases - mlp_pred$mean)^2)
  forecasts <- data.frame(forecast = mlp_pred$mean)
  return(c(forecasts, mlp_ase))}

robust_temp_forecast <- function(FUNCTION, n_ahead, target_window){
  norm_cast <- function(FUNCTION, n_ahead, target_window){
    return(FUNCTION(n_ahead, target_window))
  }
  backup_cast <- function(err){
    return(em_ts(n_ahead, target_window))
  }
  results <- tryCatch(norm_cast(FUNCTION, n_ahead, target_window), error = backup_cast )
  return(results)
}

#new with trycatch 
ase_gen <- function(target_df, FUNCTION){
  j = 12 #n ahead
  ntotal <- length(target_df$STD_Cases)#+1
  trainingSize = round(ntotal*.7)
  #60 #will cause problems with the shorter datasets if left at flat 60
  temp_roll_ase = c() #holder
  temp_ase_matrix = matrix(0, ncol = 12, nrow = length(1:(ntotal-(trainingSize + j) + 1))) #preallocation
  
  for(k in 1:(ntotal-(trainingSize + j) + 1)){
    #print(k)
    # print(substitute(FUNCTION)) #use subsitute to return function name as string
    if(substitute(FUNCTION) == 'nnc'){ #use subsitute to return function name as symbol/match
      temp_forecasts <- FUNCTION(target_df[k:(k+(trainingSize-1)),], j, 5, c(10, 10, 10), FALSE)
      for(i in 1:12){
        wstart <- trainingSize + k 
        wend <- trainingSize + k + i -1
        #print(paste(wstart, ':', wend))
        temp_ase_matrix[k, i] <- mean((target_df$STD_Cases[(wstart:wend)]-temp_forecasts$forecast[1:i])^2)
      }}
    else{
      temp_forecasts <- robust_temp_forecast(FUNCTION, j, target_df[k:(k+(trainingSize-1)),]) #FUNCTION(j, target_df[k:(k+(trainingSize-1)), ]))   #feed ar_ts dataframe with shifting rows
      for(i in 1:12){
        wstart <- trainingSize + k 
        wend <- trainingSize + k + i -1
        #print(paste(wstart, ':', wend))
        temp_ase_matrix[k, i] <- mean((target_df$STD_Cases[(wstart:wend)]-temp_forecasts[1:i])^2)
      }}
  }
  #print(k)
  
  closeAllConnections()
  for(i in 1:12){
    temp_roll_ase[i] <-mean(temp_ase_matrix[, i])}
  return(temp_roll_ase)
  #return(temp_ase_matrix)
}


##### DATA PREP
#df = read.csv("D:/SMU/DS 6120 Capstone A/appv2/merged_2013-2019.csv")
# df_13_14 = read.csv('D:/SMU/DS 6120 Capstone A/appv2/Bivin CC 2013-14_updated_headings.csv')
# df_15_16 = read.csv('D:/SMU/DS 6120 Capstone A/appv2/Bivin CC 2015-16_updated_headings.csv')
# df_17_19 = read.csv('D:/SMU/DS 6120 Capstone A/appv2/Bivin CC 2017-19_updated_headings.csv')

df_13_14 = read.csv('Bivin CC 2013-14_updated_headings.csv')
df_15_16 = read.csv('Bivin CC 2015-16_updated_headings.csv')
df_17_19 = read.csv('Bivin CC 2017-19_updated_headings.csv')

#format date column
df_17_19$date = date.mmddyy(mdy.date(match(substr(df_17_19$Month, 1, 3),month.abb),01,df_17_19$Year),sep="/")
df_15_16$date = date.mmddyy(mdy.date(match(substr(df_15_16$Month, 4, 6),month.abb),01,df_15_16$Year),sep="/")
df_13_14$date = date.mmddyy(mdy.date(match(substr(df_13_14$Month, 1, 3),month.abb),01,df_13_14$Year),sep="/")

#df <- df_13_14
df <- rbind(df_17_19,df_15_16,df_13_14)


check.for.missing.data <- function(data){
  a = colnames(data)
  b = colSums(is.na(df))  %>% as.data.table
  missing_value_table = cbind(a, b)
  colnames(missing_value_table) = c("Variables","Missing_values")
  missing_value_table = missing_value_table  %>% filter(Missing_values>0)  %>% 
    mutate("As a % of Total Values" = round(100 * (Missing_values / nrow(df)), 1))  %>% 
    arrange(desc(Missing_values))
  head(missing_value_table, 20)}
table_a = check.for.missing.data(data=df)
invisible(df %>% filter(Account_Status != "Closed"))
drops <- c("Metrics","Year","Month","House","Account_Status","Beverage_Type","Fiscal_Year","Premise","Customer_Street_Address","Customer_City",
           "Customer_Zip_Code","Longitude_Customer","Latitude_Customer","Customer","Vendor","Brand_ID","Brand","Size","Product_ID","Chain","Category",
           "Product_Type_ID","Qty_Per_Case","Alcohol_Proof","X9L_Cases","Dollar_Sales_Per_Case","Dollar_Sales_Per_9L_Case")
df = df[ , !(names(df) %in% drops)]
df$Dollar_Sales = as.numeric(gsub('[$,]', '', df$Dollar_Sales))
# Create file with all possible combinations
combinations0 = as.data.frame(df %>% group_by(Product_Type, Product, Customer_ID) %>% tally(sort=TRUE))
combinations = combinations0 %>% filter(n >= 42)


product_list = unique(df$Product)
cust_ID <- unique(combinations$Customer_ID)

#sort product names by cases sold
case_total = c()
for(i in seq(1, length(product_list), 1)) {
  case_total[i] <- sum(df[df$Product == product_list[i], ]$STD_Cases)}
product_cases <- data.frame(name = product_list, cases = case_total)
sorted_total_cases <- product_cases[order(-product_cases$cases), ]

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Aggregated Demand Forecasting"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3('Select Product Name'),
      selectInput(inputId = 'pname', 'Product', sorted_total_cases$name), 
      actionButton("b1", "Submit"),
      width = 4
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput('plot1'), 
      DT::dataTableOutput('table1'), 
      textOutput('runtime'), 
      #DT::dataTableOutput('ztable') #diagnostics
      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  rv <- reactiveValues() #assign start and end times to this global-ish var
  
  output$text1 <- renderText({
    paste('Displaying Selected Product: ', input$pname)
  })
  
  filtered_reactive <- eventReactive(input$b1, { #assign filtered df z to filtered_reactive()
    rv$startTime <- Sys.time()
    product_name <- input$pname
    combolist <- combinations[which(combinations$Product == product_name), ]
    z <- df[which(df$Product == product_name), ]
    z$date <- as.Date(z$date, '%m/%d/%Y')
    z
  })
  
  # output$ztable <- DT::renderDataTable({
  #   rv$endTime <- Sys.time()
  #   filtered_reactive()
  # })
  
  prediction_reactive <- eventReactive(input$b1, {
    product_name <- input$pname

    withProgress(message = 'Making plot', value = 0, {
      combolist <- combinations[which(combinations$Product == product_name), ]
      z <- filtered_reactive() #stand in for z
      proto_df_list = list()
      for(i in seq(1, length(combolist$Customer_ID), 1)){
        #print(combolist$Customer_ID[i])
        y = z[which(z$Customer_ID == combolist$Customer_ID[i]), ]
        proto_df_list[[i]] <- y
      }
      #date to line up each prod/cust combo
      full_date_range = seq(from = min(as.Date(df$date, '%m/%d/%Y')), to = max(as.Date(df$date, '%m/%d/%Y')), by = 'month')
      #get full range of dates to 'expand' and reveal missing data
      df_template <- data.frame(date = full_date_range)
      df_list = list()#store fixed df here
      for(f in 1:length(proto_df_list)){
        mtest <- data.frame(left_join(df_template, proto_df_list[[f]], by = 'date'))
        mtest[is.na(mtest)] <- 0
        df_list[[f]] <- mtest
      }
      #gen ase holders and forecast holders
      modelnames = c('EqualMeans', 'AR', 'ARMA', 'ARI', 'ARIMA', 'ARI_S12', 'ARIMA_S12', 'RF', 'MLP')
      modelnames2 <- c('Actual', paste(modelnames, '_ASE'), paste(modelnames, '_F'))
      #forecast storage
      r_fcast <- matrix(data = 0, nrow = 12, ncol = (length(modelnames) + 1)) %>% as.data.frame()
      names(r_fcast) <- c('actual', modelnames)
      #ase storage
      r_ase <- matrix(data = 0, nrow = 12, ncol = length(modelnames)) %>% as.data.frame()
      names(r_ase) <- modelnames
      #store future predictions and winning model its based on per cust ID
      iter_holder <- data.frame(matrix(data = 0, ncol = length(df_list), nrow = 12+2)) #12 preds + 2 ids
      ###END PRODUCT SELECTION SECTOR###

      ###FORECASTING SECTION BOOT###
      for(s in 1:length(df_list)){ #target assignment, cycle through df_list per custID
      #for(s in 1:1){ #testing truncation
        incProgress(1/s, detail = paste("Doing part", s))
        target_df = df_list[[s]]
        target_df$date <- as.Date(target_df$date, '%m/%d/%Y') #Date fix, char form messes with ordering
        target_df <- target_df[order(target_df$date), ]
        trainingSize = round(length(target_df$Customer_ID)*.7)
        #trainingSize = 60
        j = 12
        r_fcast['actual'] <- target_df$STD_Cases[trainingSize:(trainingSize+j-1)]
        #take the column name at match it to function, gen ASE, forecasts, and winning model
        for(i in names(r_ase)){
          #sink()
          if(i == 'EqualMeans'){r_ase[i] <- ase_gen(target_df, em_ts)
          r_fcast[i] <- em_ts(12, target_df)}
          else if(i == 'AR'){r_ase[i] <- ase_gen(target_df, ar_ts_boot)
          r_fcast[i] <- ar_ts_boot(12, target_df)}
          else if(i == 'ARMA'){r_ase[i] <- ase_gen(target_df, arma_ts_boot)
          r_fcast[i] <- arma_ts_boot(12, target_df)}
          else if(i == 'ARI'){r_ase[i] <- ase_gen(target_df, ari_ts_boot)
          r_fcast[i] <- ari_ts_boot(12, target_df)}
          else if(i == 'ARIMA'){r_ase[i] <- ase_gen(target_df, arima_ts_boot)
          r_fcast[i] <- arima_ts_boot(12, target_df)}
          else if(i == 'ARI_S12'){r_ase[i] <- ase_gen(target_df, ari_s12_ts_boot)
          r_fcast[i] <- ari_s12_ts_boot(12, target_df)}
          else if(i == 'ARIMA_S12'){r_ase[i] <- ase_gen(target_df, arima_s12_ts_boot)
          r_fcast[i] <- arima_s12_ts_boot(12, target_df)}
          else if(i == 'RF'){r_ase[i] <- ase_gen(target_df, rf_ts)
          r_fcast[i] <- rf_ts(12, target_df)}
          else if(i == 'MLP'){r_ase[i] <- ase_gen(target_df, nnc)
          r_fcast[i] <- nnc(target_df, 12, 10, c(10, 10, 10), FALSE)}
          else{break()
            #print('unrecognized model name')
          }}

        mean_roll_ase_holder = c()
        c = 1
        for(i in names(r_ase)){
          #print( sum((r_fcast['actual']-r_fcast[i])^2)/length(r_fcast[1,]) )
          mean_roll_ase_holder[c] <- sum((r_fcast['actual']-r_fcast[i])^2)/length(r_fcast[1,])
          c = c + 1}
        mean_ase_frame <- data.frame(model_name = names(r_ase), mean_rolling_ase = mean_roll_ase_holder)# %>% t()
        #simple forecast using best ASE for $input n_ahead
        #overall winning model
        gen_winner <- mean_ase_frame$model_name[which.min(mean_ase_frame$mean_rolling_ase)]
        #basic forecast, starting at 12 out from total data
        future_ahead = 12
        if(gen_winner == 'EqualMeans'){temp_cust_forecast = em_ts(future_ahead, target_df)
        }else if(gen_winner == 'AR'){temp_cust_forecast = ar_ts_boot(future_ahead, target_df)
        }else if(gen_winner == 'ARMA'){temp_cust_forecast = arma_ts_boot(future_ahead, target_df)
        }else if(gen_winner == 'ARI'){temp_cust_forecast = ari_ts_boot(future_ahead, target_df)
        }else if(gen_winner == 'ARIMA'){temp_cust_forecast = arima_ts_boot(future_ahead, target_df)
        }else if(gen_winner == 'ARI_S12'){temp_cust_forecast = ari_s12_ts_boot(future_ahead, target_df)
        }else if(gen_winner == 'ARIMA_S12'){temp_cust_forecast = arima_s12_ts_boot(future_ahead, target_df)
        }else if(gen_winner == 'RF'){temp_cust_forecast = rf_ts(future_ahead, target_df)
        }else if(gen_winner == 'MLP'){temp_cust_forecast = nnc(target_df, future_ahead, 10, c(10, 10, 10), FALSE)$forecast
        }else{break()
          print('unknown model type')
        }
        #generates forecasts with the selected model for this particular customerID, loop this for however many customerIDs
        iter_loop <- c(target_df$Customer_ID[1], gen_winner, temp_cust_forecast)
        iter_holder[, s] <- iter_loop
        pred_dates <- seq(tail(target_df$date, 1), by = 'month', length.out = 12 + 1)[2:13]
        #sequence of dates n spaces out from the last predictions of the target df
        #includes starting date, but cut off for storage
      }


      #aggregate forecasts
      #cust_results <- iter_holder %>% t() %>% as.data.frame()
      cust_results = as.data.frame(t(iter_holder))
      names(cust_results) <- c('cust_id', 'winning_model', 'win1', 'win2', 'win3', 'win4', 'win5', 'win6', 'win7', 'win8', 'win9', 'win10', 'win11', 'win12')
      final_results <- c()
      for(r in 1:12){
        final_results[r] <- sum(as.numeric(cust_results[ ,(r+2)])) #no rounding
      }
      #add prediction/date frame to end and add category for plotting
      predictions = data.frame(date = pred_dates, cases = final_results)
    }) #end with reactive bar thing
    #rv$predictions <- predictions#saves dataframe predictions as prediction_reactive() to call elsewhere
    rv$endTime <- Sys.time() #note, order is important, the last item is saved as the reactive item (i think) 
    predictions
  })

  output$table1 = DT::renderDataTable({
    prediction_reactive() 
    #rv$predictions
   }, options = list(pageLength = 15, info = FALSE,
                      lengthMenu = list(c(3, 6, -1), c("3", '6', "All"))), 
   class = 'cell-border stripe')

  output$plot1 <- renderPlot({
    predictions <- prediction_reactive()
    predictions$id = rep('prediction', length(predictions$date))

    #aggregate all actual cases by date
    z = filtered_reactive()
    dlist = sort(unique(z$date))
    clist = c()
    i = 1
    for(d in dlist){
      clist[i] <- sum(z[z$date == d, ]$STD_Cases)
      i = i + 1}
    actuals = data.frame(date = dlist, cases = clist)
    actuals$id = rep('actual', length(actuals$date))
    #final viz output - actual data + simple prediction line
    final_data = rbind(actuals, predictions) #hook to reactive prediction bit here
    names(final_data) = c('date', 'cases', 'id')

    #will final_data even show up as DT?
    v1 = ggplot(final_data, aes(x = date, y = cases, color = id), environment = environment()) +
      geom_line() +
      ggtitle(paste('12 Month Forecast:', input$pname)) + xlab('Date') + ylab('Cases') +
      labs(color = '') +
      #theme_minimal() +
      theme_light()+
      theme(panel.grid.minor = element_line(color = 'white'),
            panel.grid.major = element_line(color = 'white'),
            axis.ticks = element_line(size = 1, color="black"), 
            text = element_text(size=20))
    #}
    plot(v1)
    #final_data %>% as.data.table()
    }) #end output plot1
  
  output$runtime <- renderText({
    print(rv$endTime - rv$startTime)
    #typeof(rv$startTime)
  })
  
} #end server

# Run the application 
shinyApp(ui = ui, server = server)
