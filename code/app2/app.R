#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("date")
library('ranger')
library('randomForest')
library('caret')
#library('tswge')
library('DT')

source('script_storage.R')
##### DATA PREP
#df = read.csv("D:/SMU/DS 6120 Capstone A/appv2/merged_2013-2019.csv")
df = read.csv("merged_2013-2019.csv")
df$date = date.mmddyy(mdy.date(match(substr(df$Month, 4, 6),month.abb),01,df$Year),sep="/") #not sure why original had different substring matches for different years, check for errors later
invisible(df %>% filter(Account_Status != "Closed")) #remove inactive accounts

#assuming all dollar sales that are NA are actually zero
df$Dollar_Sales[is.na(df$Dollar_Sales)] <- 0
df$Dollar_Sales = as.numeric(gsub('[$,]', '', df$Dollar_Sales)) #format $$$

#remove unused variables
df_copy <- df #but make a copy of original for VAR later?

drops <- c("Metrics","Year","Month","House","Account_Status","Beverage_Type","Fiscal_Year","Premise","Customer_Street_Address","Customer_City",
           "Customer_Zip_Code","Longitude_Customer","Latitude_Customer","Customer","Vendor","Brand_ID","Brand","Size","Product_ID","Chain","Category",
           "Product_Type_ID","Qty_Per_Case","Alcohol_Proof","X9L_Cases","Dollar_Sales_Per_Case","Dollar_Sales_Per_9L_Case")
df = df[ , !(names(df) %in% drops)] #drop vars

#create unique combinations of product/custID
combinations0 = as.data.frame(df %>% group_by(Product_Type, Product, Customer_ID) %>% tally(sort=TRUE))
combinations = combinations0 %>% filter(n >= 42) #filter out TS that have less than 42 purchases over 7 years

product_list = unique(df$Product)
cust_ID <- unique(combinations$Customer_ID)

#sort product names by cases sold
case_total = c()
for(i in seq(1, length(product_list), 1)) {
  case_total[i] <- sum(df[df$Product == product_list[i], ]$STD_Cases)}
product_cases <- data.frame(name = product_list, cases = case_total)
sorted_total_cases <- product_cases[order(-product_cases$cases), ]


##### END DATA PREP

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Model Selection Example"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 5, 
      #selectInput(inputId = 'cname', 'Sort By (currently nonfunctional)', names(df)), #hook this into the sorting part?
      # radioButtons(inputId = 'rchoice', 'Sort By', choices = c('Alphabetical' = 'Product', 
      #                                                          'Cases' = 'STD_Cases', 
      #                                                          'Sales'  = 'Dollar_Sales')),
      selectInput(inputId = 'pname', 'Product', sorted_total_cases)#, 
      #sliderInput(inputId = 'n_ahead', "Forcast", min = 1, max = 12, value = 1, animate = TRUE)
      #uiOutput('p_list')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #plotOutput("distPlot")
      textOutput('test1'),
      #textOutput('test2'), 
      #DT::dataTableOutput('t1'),
      plotOutput('plot1')
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  
  # output$test1 <- renderTable({
  #     temp_df <- df[df$Product == input$pname, ]
  #     head(temp_df)
  #    #ok so need to do the whole aggregation estimation thing for each temp df as cust ID iterates
  # })
  
  output$test1 <- renderText({
    #pred_holder = c() #holder for predictions
    #for(i in seq(1, length(cust_ID), 1)){
    #    temp_df <- df[df$Product == input$pname & df$Customer_ID == cust_ID[i], ]
    #}
    input$pname
    #total_aggregate_results = 35
    #paste('total forecast results go here: ', total_aggregate_results)
  })
  
  
  # test_return <- reactive({
  #   product_name <- input$pname
  #   combolist <- combinations[which(combinations$Product == product_name), ]
  #   z <- df[which(df$Product == product_name), ]
  #   z$date <- as.Date(z$date, '%m/%d/%Y')
  #   proto_df_list = list()
  #   for(i in seq(1, length(combolist$Customer_ID), 1)){
  #         print(combolist$Customer_ID[i])
  #         y = z[which(z$Customer_ID == combolist$Customer_ID[i]), ]
  #         proto_df_list[[i]] <- y
  #         #use df_list[[n]] to access each dataframe in format
  #       }
  # })
  
  # output$t1 = DT::renderDataTable({
  # #output$t1 = renderText({
  #   #test_return()
  #   product_name <- input$pname
  #   combolist <- combinations[which(combinations$Product == product_name), ]
  #   z <- df[which(df$Product == product_name), ]
  #   z$date <- as.Date(z$date, '%m/%d/%Y')
  #   proto_df_list = list()
  #   for(i in seq(1, length(combolist$Customer_ID), 1)){
  #     #print(combolist$Customer_ID[i])
  #     y = z[which(z$Customer_ID == combolist$Customer_ID[i]), ]
  #     proto_df_list[[i]] <- y
  #   }
  #   #proto_df_list[[1]] #ok so dt render data table will show a df
  #   
  #   #date to line up each prod/cust combo
  #   full_date_range = seq(from = min(as.Date(df$date, '%m/%d/%Y')), to = max(as.Date(df$date, '%m/%d/%Y')), by = 'month')
  #   #get full range of dates to 'expand' and reveal missing data
  #   df_template <- data.frame(date = full_date_range)
  #   df_list = list()#store fixed df here
  #   for(f in 1:length(proto_df_list)){
  #     mtest <- data.frame(left_join(df_template, proto_df_list[[f]], by = 'date'))
  #     mtest[is.na(mtest)] <- 0
  #     df_list[[f]] <- mtest
  #   }#This makes df's date-contigous
  #   #df_list[[1]] #also shows
  #   #gen ase holders and forecast holders
  #   modelnames = c('EqualMeans', 'AR', 'ARMA', 'ARI', 'ARIMA', 'ARI_S12', 'ARIMA_S12', 'RF', 'MLP')
  #   modelnames2 <- c('Actual', paste(modelnames, '_ASE'), paste(modelnames, '_F'))
  #   #forecast storage
  #   r_fcast <- matrix(data = 0, nrow = 12, ncol = (length(modelnames) + 1)) %>% as.data.frame()
  #   names(r_fcast) <- c('actual', modelnames)
  #   #ase storage
  #   r_ase <- matrix(data = 0, nrow = 12, ncol = length(modelnames)) %>% as.data.frame()
  #   names(r_ase) <- modelnames
  #   
  #   #store misc stuff here
  #   misc_holder <- c()
  #   #store future predictions and winning model its based on per cust ID
  #   iter_holder <- data.frame(matrix(data = 0, ncol = length(df_list), nrow = 12+2)) #12 preds + 2 ids
  #   ###END PRODUCT SELECTION SECTOR###
  #   
  #   ###FORECASTING SECTION###
  #   for(s in 1:length(df_list)){ #target assignment, cycle through df_list per custID
  #     target_df = df_list[[s]]
  #     target_df$date <- as.Date(target_df$date, '%m/%d/%Y') #Date fix, char form messes with ordering
  #     target_df <- target_df[order(target_df$date), ]
  #     trainingSize = round(length(target_df$Customer_ID)*.7)
  #     #trainingSize = 60
  #     j = 12
  #     r_fcast['actual'] <- target_df$STD_Cases[trainingSize:(trainingSize+j-1)]
  #     #take the column name at match it to function, gen ASE, forecasts, and winning model
  #     for(i in names(r_ase)){
  #       #sink()
  #       if(i == 'EqualMeans'){r_ase[i] <- ase_gen(target_df, em_ts)
  #       r_fcast[i] <- em_ts(12, target_df)}
  #       #else if(i == 'AR'){r_ase[i] <- ase_gen(target_df, ar_ts_boot)
  #       else if(i == 'AR'){r_ase[i] <- ase_gen(target_df, ar_ts)
  #       r_fcast[i] <- ar_ts(12, target_df)}
  #       #else if(i == 'ARMA'){r_ase[i] <- ase_gen(target_df, arma_ts_boot)
  #       else if(i == 'ARMA'){r_ase[i] <- ase_gen(target_df, arma_ts)
  #       r_fcast[i] <- arma_ts(12, target_df)}
  #       #else if(i == 'ARI'){r_ase[i] <- ase_gen(target_df, ari_ts_boot)
  #       else if(i == 'ARI'){r_ase[i] <- ase_gen(target_df, ari_ts)
  #       r_fcast[i] <- ari_ts(12, target_df)}
  #       #else if(i == 'ARIMA'){r_ase[i] <- ase_gen(target_df, arima_ts_boot)
  #       else if(i == 'ARIMA'){r_ase[i] <- ase_gen(target_df, arima_ts)
  #       r_fcast[i] <- arima_ts(12, target_df)}
  #       #else if(i == 'ARI_S12'){r_ase[i] <- ase_gen(target_df, ari_ts_boot)
  #       else if(i == 'ARI_S12'){r_ase[i] <- ase_gen(target_df, ari_ts)
  #       r_fcast[i] <- ari_s12_ts(12, target_df)}
  #       #else if(i == 'ARIMA_S12'){r_ase[i] <- ase_gen(target_df, arima_s12_ts_boot)
  #       else if(i == 'ARIMA_S12'){r_ase[i] <- ase_gen(target_df, arima_s12_ts)
  #       r_fcast[i] <- arima_s12_ts(12, target_df)}
  #       else if(i == 'RF'){r_ase[i] <- ase_gen(target_df, rf_ts)
  #       r_fcast[i] <- rf_ts(12, target_df)}
  #       else if(i == 'MLP'){r_ase[i] <- ase_gen(target_df, nnc)
  #       r_fcast[i] <- nnc(target_df, 12, 10, c(10, 10, 10), FALSE)}
  #       else{break()
  #         #print('unrecognized model name')
  #       }}
  #     
  #     mean_roll_ase_holder = c()
  #     c = 1
  #     for(i in names(r_ase)){
  #       #print( sum((r_fcast['actual']-r_fcast[i])^2)/length(r_fcast[1,]) )
  #       mean_roll_ase_holder[c] <- sum((r_fcast['actual']-r_fcast[i])^2)/length(r_fcast[1,])
  #       c = c + 1}
  #     mean_ase_frame <- data.frame(model_name = names(r_ase), mean_rolling_ase = mean_roll_ase_holder)# %>% t()
  #     #simple forecast using best ASE for $input n_ahead
  #     #overall winning model
  #     gen_winner <- mean_ase_frame$model_name[which.min(mean_ase_frame$mean_rolling_ase)]
  #     #basic forecast, starting at 12 out from total data
  #     future_ahead = 12
  #     if(gen_winner == 'EqualMeans'){temp_cust_forecast = em_ts(future_ahead, target_df)
  #     }else if(gen_winner == 'AR'){temp_cust_forecast = ar_ts(future_ahead, target_df)
  #     }else if(gen_winner == 'ARMA'){temp_cust_forecast = arma_ts(future_ahead, target_df)
  #     }else if(gen_winner == 'ARI'){temp_cust_forecast = ari_ts(future_ahead, target_df)
  #     }else if(gen_winner == 'ARIMA'){temp_cust_forecast = arima_ts(future_ahead, target_df)
  #     }else if(gen_winner == 'ARI_S12'){temp_cust_forecast = ari_s12_ts(future_ahead, target_df)
  #     }else if(gen_winner == 'ARIMA_S12'){temp_cust_forecast = arima_s12_ts(future_ahead, target_df)
  #     }else if(gen_winner == 'RF'){temp_cust_forecast = rf_ts(future_ahead, target_df)
  #     }else if(gen_winner == 'MLP'){temp_cust_forecast = nnc(target_df, future_ahead, 10, c(10, 10, 10), FALSE)$forecast
  #     }else{break()
  #       print('unknown model type')
  #     }
  #     #generates forecasts with the selected model for this particular customerID, loop this for however many customerIDs
  #     iter_loop <- c(target_df$Customer_ID[1], gen_winner, temp_cust_forecast)
  #     iter_holder[, s] <- iter_loop
  #     pred_dates <- seq(tail(target_df$date, 1), by = 'month', length.out = 12 + 1)[2:13]
  #     #sequence of dates n spaces out from the last predictions of the target df
  #     #includes starting date, but cut off for storage
  #   }
  #   #aggregate forecasts
  #   cust_results <- iter_holder %>% t() %>% as.data.frame()
  #   #cust_results = as.data.frame(iter_holder)
  #   names(cust_results) <- c('cust_id', 'winning_model', 'win1', 'win2', 'win3', 'win4', 'win5', 'win6', 'win7', 'win8', 'win9', 'win10', 'win11', 'win12')
  #   final_results <- c()
  #   for(r in 1:12){
  #     #print(r)
  #     final_results[r] = sum(as.numeric(cust_results[,(r+2)])) #no rounding
  #     #print(cust_results[,(r+2)])
  #   }
  #   #final_results %>% as.data.table()
  #   #aggregate all actual cases by date
  #   dlist <- sort(unique(z$date))
  #   clist = c()
  #   i = 1
  #   for(d in dlist){
  #     clist[i] = sum(z[z$date == d, ]$STD_Cases)
  #     i = i + 1}
  #   actuals = data.frame(date = dlist, cases = clist)
  #   actuals$id = rep('actual', length(actuals$date))
  #   #add prediction/date frame to end and add category for plotting
  #   predictions = data.frame(date = pred_dates, cases = final_results)
  #   predictions$id = rep('prediction', length(predictions$date))
  #   
  #   #final viz output - actual data + simple prediction line
  #   final_data = rbind(actuals, predictions)
  #   names(final_data) = c('date', 'cases', 'id')
  #   final_data %>% as.data.table
  # })
  
  #maybe can print final_results in renderUI? 
  
  ###PLOT OUTPUT###
  
  output$plot1 <- renderPlot({
    product_name <- input$pname
    combolist <- combinations[which(combinations$Product == product_name), ]
    z <- df[which(df$Product == product_name), ]
    z$date <- as.Date(z$date, '%m/%d/%Y')
    proto_df_list = list()
    for(i in seq(1, length(combolist$Customer_ID), 1)){
      #print(combolist$Customer_ID[i])
      y = z[which(z$Customer_ID == combolist$Customer_ID[i]), ]
      proto_df_list[[i]] <- y
    }
    #proto_df_list[[1]] #ok so dt render data table will show a df

    #date to line up each prod/cust combo
    full_date_range = seq(from = min(as.Date(df$date, '%m/%d/%Y')), to = max(as.Date(df$date, '%m/%d/%Y')), by = 'month')
    #get full range of dates to 'expand' and reveal missing data
    df_template <- data.frame(date = full_date_range)
    df_list = list()#store fixed df here
    for(f in 1:length(proto_df_list)){
      mtest <- data.frame(left_join(df_template, proto_df_list[[f]], by = 'date'))
      mtest[is.na(mtest)] <- 0
      df_list[[f]] <- mtest
    }#This makes df's date-contigous
    #df_list[[1]] #also shows
    #gen ase holders and forecast holders
    modelnames = c('EqualMeans', 'AR', 'ARMA', 'ARI', 'ARIMA', 'ARI_S12', 'ARIMA_S12', 'RF', 'MLP')
    modelnames2 <- c('Actual', paste(modelnames, '_ASE'), paste(modelnames, '_F'))
    #forecast storage
    r_fcast <- matrix(data = 0, nrow = 12, ncol = (length(modelnames) + 1)) %>% as.data.frame()
    names(r_fcast) <- c('actual', modelnames)
    #ase storage
    r_ase <- matrix(data = 0, nrow = 12, ncol = length(modelnames)) %>% as.data.frame()
    names(r_ase) <- modelnames

    #store misc stuff here
    misc_holder <- c()
    #store future predictions and winning model its based on per cust ID
    iter_holder <- data.frame(matrix(data = 0, ncol = length(df_list), nrow = 12+2)) #12 preds + 2 ids
    ###END PRODUCT SELECTION SECTOR###

    ###FORECASTING SECTION###
    for(s in 1:length(df_list)){ #target assignment, cycle through df_list per custID
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
        #else if(i == 'AR'){r_ase[i] <- ase_gen(target_df, ar_ts_boot)
        else if(i == 'AR'){r_ase[i] <- ase_gen(target_df, ar_ts)
        r_fcast[i] <- ar_ts(12, target_df)}
        #else if(i == 'ARMA'){r_ase[i] <- ase_gen(target_df, arma_ts_boot)
        else if(i == 'ARMA'){r_ase[i] <- ase_gen(target_df, arma_ts)
        r_fcast[i] <- arma_ts(12, target_df)}
        #else if(i == 'ARI'){r_ase[i] <- ase_gen(target_df, ari_ts_boot)
        else if(i == 'ARI'){r_ase[i] <- ase_gen(target_df, ari_ts)
        r_fcast[i] <- ari_ts(12, target_df)}
        #else if(i == 'ARIMA'){r_ase[i] <- ase_gen(target_df, arima_ts_boot)
        else if(i == 'ARIMA'){r_ase[i] <- ase_gen(target_df, arima_ts)
        r_fcast[i] <- arima_ts(12, target_df)}
        #else if(i == 'ARI_S12'){r_ase[i] <- ase_gen(target_df, ari_ts_boot)
        else if(i == 'ARI_S12'){r_ase[i] <- ase_gen(target_df, ari_ts)
        r_fcast[i] <- ari_s12_ts(12, target_df)}
        #else if(i == 'ARIMA_S12'){r_ase[i] <- ase_gen(target_df, arima_s12_ts_boot)
        else if(i == 'ARIMA_S12'){r_ase[i] <- ase_gen(target_df, arima_s12_ts)
        r_fcast[i] <- arima_s12_ts(12, target_df)}
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
      }else if(gen_winner == 'AR'){temp_cust_forecast = ar_ts(future_ahead, target_df)
      }else if(gen_winner == 'ARMA'){temp_cust_forecast = arma_ts(future_ahead, target_df)
      }else if(gen_winner == 'ARI'){temp_cust_forecast = ari_ts(future_ahead, target_df)
      }else if(gen_winner == 'ARIMA'){temp_cust_forecast = arima_ts(future_ahead, target_df)
      }else if(gen_winner == 'ARI_S12'){temp_cust_forecast = ari_s12_ts(future_ahead, target_df)
      }else if(gen_winner == 'ARIMA_S12'){temp_cust_forecast = arima_s12_ts(future_ahead, target_df)
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
    #aggregate all actual cases by date
    dlist <- sort(unique(z$date))
    clist = c()
    i = 1
    for(d in dlist){
      clist[i] <- sum(z[z$date == d, ]$STD_Cases)
      i = i + 1}
    actuals = data.frame(date = dlist, cases = clist)
    actuals$id = rep('actual', length(actuals$date))
    #add prediction/date frame to end and add category for plotting
    predictions = data.frame(date = pred_dates, cases = final_results)
    predictions$id = rep('prediction', length(predictions$date))

    #final viz output - actual data + simple prediction line
    final_data = rbind(actuals, predictions)
    names(final_data) = c('date', 'cases', 'id')
    #will final_data even show up as DT?
    #
    # vplot <- function(fdf){
    v1 = ggplot(final_data, aes(x = date, y = cases, color = id), environment = environment()) +
      geom_line() +
      ggtitle(paste('12 Month Forecast:', product_name)) + xlab('Date') + ylab('Cases') +
      labs(color = '')
    #}
    plot(v1)
    #final_data %>% as.data.table()
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
