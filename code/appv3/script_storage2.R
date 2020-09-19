library(rmdformats)
library(tidyverse)  # data manipulaiton
library(data.table)
#library(tswge)  # Time series package
library(tseries)  # for Dickey-Fuller test 
library(orcutt)  # for Cochrane-Orcutt test
library(formattable)  # for table formatting
library(GGally)
library(astsa)
library(nnfor)
library(dplyr)
library(ggplot2)
library(changepoint)
library(date)
library(R.devices)


#Equal Means
#accept df object and return forcasts 1:12
em_ts <- function(n_ahead, target_df){
  return(rep((mean(target_df$STD_Cases)),  n_ahead))}

#bootleg AIC5 with no tswge
#target_vector <- target_df$STD_Cases 
aic_boot <- function(target_vector){
  pqc <- expand.grid(0:5, 0:2)
  aic_holder <- vector()
  p_holder <- vector()
  q_holder <- vector()
  for(i in 1:length(pqc$Var1)) {
    temp_model <- arima(target_vector, order = c(pqc[i, 1], 0, pqc[i, 2]))
    #aic_holder <- append(aic_holder, temp_model$aic) #base AIC with k = 2?
    aic_holder[i] <- AIC(temp_model, k = log(length(target_vector)))
    p_holder <- append(p_holder, pqc[i, 1])
    q_holder <- append(q_holder, pqc[i, 2])}
  aic_results <- data.frame(p = p_holder, q = q_holder, aic = aic_holder)
  aic_results <- aic_results[order(aic_results$aic), ]
  return(head(aic_results, 1))
}

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
  return(head(aic_results, 1))
}

# #AR #use commented sections if tswge works
# ar_ts <- function(n_ahead, target_df){
#   model1 = aic.wge(target_df$STD_Cases ,q=0,type="aic") %>% invisible()
#   if (model1$p == 0){
#     newphi = 1
#   } else {
#     newphi = model1$p
#   }
#   model1_est = est.ar.wge(target_df$STD_Cases ,p=newphi, factor = FALSE) %>% invisible()
#   forecasts = fore.aruma.wge(target_df$STD_Cases,phi = model1_est$phi, theta = 0, s = 0, d = 0,n.ahead = n_ahead,plot=FALSE) %>% invisible()
#   return(forecasts$f)
# }
#looks to work, but prints several polynomial coefficient output
#ar_ts(5, target_df) 

#AR model bootleg
ar_ts_boot <- function(n_ahead, target_df){
  aic_temp <- aic_boot2(target_df$STD_Cases)
  if (aic_temp$p == 0){
    newphi = 1}
  else{newphi = aic_temp$p}
  model_temp <- arima0(target_df$STD_Cases, order = c(newphi, 0, 0), method = 'ML') #note q is zero for AR model
  pred_temp <- invisible(predict(model_temp, n_ahead))
  return(pred_temp$pred)
}
#ar_ts_boot(5, target_df) #close enough

#ARMA
# arma_ts <- function(n_ahead, target_df){
#   model1 = aic.wge(target_df$STD_Cases,type="aic") %>% invisible()
#   model1_est = est.arma.wge(target_df$STD_Cases,p=model1$p,q=model1$q) %>% invisible()
#   forecasts = fore.aruma.wge(target_df$STD_Cases,phi = model1_est$phi, theta = model1_est$theta, s = 0, d = 0,n.ahead = n_ahead,plot=FALSE)
#   return(forecasts$f)} #can't hide coefficient output by assigning to var ??
# #arma_ts(8, target_df)

arma_ts_boot <- function(n_ahead, target_df){
  aic_temp <- aic_boot2(target_df$STD_Cases)
  if (aic_temp$p == 0){
    newphi = 1}
  else newphi = aic_temp$p #error if inside curly brackets??? 
  model_temp <- arima0(target_df$STD_Cases, order = c(aic_temp$p, 0, newphi)) #note q is not zero for ARMA model
  pred_temp <- predict(model_temp, n_ahead) %>% invisible()
  return(pred_temp$pred)
}
#arma_ts_boot(8, target_df)

#ARI (q = 0 and d = 1)
# ari_ts <- function(n_ahead, target_df)
# {
#   temp2 = artrans.wge(target_df$STD_Cases, 1) %>% invisible() #take difference
#   model1 = aic.wge(temp2,q=0,type="aic") %>% invisible()
#   if (model1$p == 0){
#     newphi = 1
#   }else{newphi = model1$p}  
#   model1_est = est.ar.wge(temp2,p=newphi) %>% invisible() #won't work will all zero?
#   forecasts = fore.aruma.wge(target_df$STD_Cases,phi = model1_est$phi, theta = 0, s = 0, d = 1,n.ahead = n_ahead,plot=FALSE) %>% invisible()
#   return(forecasts$f)
# }
# #ari_ts(12, target_df)

#ARI no tswge
ari_ts_boot <- function(n_ahead, target_df){
  diff1 <- diff(target_df$STD_Cases)
  aic_temp <- aic_boot2(diff1)
  if (aic_temp$p == 0){
    newphi = 1}
  else{newphi = aic_temp$p}
  pred_temp <- predict(arima0(target_df$STD_Cases, order = c(newphi, 1, 0)), n.ahead = n_ahead)
  return(pred_temp$pred)
}
#ari_ts_boot(12, target_df) #close enough

# #ARIMA (with d = 1?)
# arima_ts <- function(n_ahead, target_df){
#   diff1 = artrans.wge(target_df$STD_Cases, 1) %>% invisible()
#   aic_temp = aic.wge(diff1, type = 'aic') %>% invisible()
#   model_temp <- est.arma.wge(diff1, p = aic_temp$p, q = aic_temp$q) %>% invisible()
#   pred_temp <- fore.aruma.wge(target_df$STD_Cases, phi = model_temp$phi, theta = model_temp$theta, s = 0, d = 1, n.ahead = n_ahead, plot = FALSE)
#   return(pred_temp$f)
# }
# #arima_ts(4, target_df)

#no tswge
arima_ts_boot <- function(n_ahead, target_df){
  diff1 = diff(target_df$STD_Cases)
  aic_temp <- aic_boot2(diff1)
  if (aic_temp$p == 0){
    newphi = 1}
  else{newphi = aic_temp$p}
  pred_temp <- predict(arima0(target_df$STD_Cases, order = c(newphi, 1, aic_temp$q)), n.ahead = n_ahead)
  return(pred_temp$pred)
}
#arima_ts_boot(4, target_df) #it's off by ~10%? 

# #ARI_S12 #(p, 1, 0), s = 12
# ari_s12_ts <- function(n_ahead, target_df){
#   seasonal1 <- artrans.wge(target_df$STD_Cases,phi.tr=c(rep(0,11),1))
#   aic_temp <- aic.wge(seasonal1, q = 0, type = 'aic')
#   if (aic_temp$p == 0){
#     newphi = 1
#   } else {
#     newphi = aic_temp$p
#   } 
#   model_temp <- est.ar.wge(seasonal1, p = newphi) 
#   pred_temp <- fore.aruma.wge(target_df$STD_Cases, phi = model_temp$phi, theta = 0, s = 12, d = 0, n.ahead = n_ahead, plot = FALSE)
#   
#   return(pred_temp$f)
# }
#ari_s12_ts(5, target_df)

#bootleg ARI_S12
ari_s12_ts_boot <- function(n_ahead, target_df){
  diff1 <- diff(target_df$STD_Cases)
  aic_temp <- aic_boot2(diff1)
  if (aic_temp$p == 0){
    newphi = 1
  } else {
    newphi = aic_temp$p
  } 
  #using astsa library sarima? 
  #pred_temp <- sarima.for((target_df$STD_Cases), n_ahead, p = newphi, d = 0, q = 0, P = 0, D = 0, Q = 0, S = 12) %>% invisible
  pred_temp <- predict(arima0(target_df$STD_Cases, order = c(newphi, 0, 0), seasonal = c(0, 0, 12)), n.ahead = 12)
  return(pred_temp$pred) }
#ari_s12_ts_boot(5, target_df) #+2% off from the tswge function

# #ARIMA_S12 #(p, 0, q), s = 12
# arima_s12_ts <- function(n_ahead, target_df){
#   aic_temp <- aic.wge(target_df$STD_Cases, type = 'aic')
#   if (aic_temp$p == 0){
#     newphi = 1
#   } else {
#     newphi = aic_temp$p
#   } 
#   model_temp <- est.arma.wge(target_df$STD_Cases, p = newphi, q = aic_temp$q) 
#   pred_temp <- fore.aruma.wge(target_df$STD_Cases, phi = model_temp$phi, theta = model_temp$theta, s = 12, d = 0, n.ahead = n_ahead, plot = FALSE)
#   
#   return(pred_temp$f)
# }
#arima_s12_ts(5, target_df)

#bootleg ARIMA_S12
arima_s12_ts_boot <- function(n_ahead, target_df){
  diff1 <- diff(target_df$STD_Cases)
  aic_temp <- aic_boot2(diff1)
  if (aic_temp$p == 0){
    newphi = 1
  } else {
    newphi = aic_temp$p
  } 
  #using astsa library sarima? 
  #pred_temp <- sarima.for((target_df$STD_Cases), n_ahead, p = newphi, d = 0, q = aic_temp$q, P = 0, D = 0, Q = 0, S = 12) %>% invisible
  pred_temp <- predict(arima0(target_df$STD_Cases, order = c(newphi, 0, aic_temp$q), seasonal = c(0, 0, 12)), n.ahead = 12)
  return(pred_temp$pred) }
#arima_s12_ts_boot(5, target_df) #+2% off from the tswge function

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
  return(results$forecast)
}

#nn complete
nnc <- function(target_df, n_ahead, reps, hd_vector, display){
  #date_line <- as.Date(target_df$date, '%m/%d/%Y') #add date back in after it works
  tlen <- length(target_df$STD_Cases)
  sample_train <- target_df[1:(tlen-n_ahead), ]
  sample_test <- target_df[(tlen-n_ahead +1):tlen, ]
  
  ts_train<- as.ts(sample_train$STD_Cases)
  fit_mlp <- mlp(ts_train, reps = reps, hd = hd_vector)
  
  mlp_pred <- forecast(fit_mlp, h = n_ahead)
  mlp_ase <- mean((sample_test$STD_Cases - mlp_pred$mean)^2)
  
  #r1 <- ggplot() + 
  #  geom_line(aes(x = sort(target_df$date), y = target_df$STD_Cases), col = 'black') +
  #  geom_line(aes(x = sort(target_df$date), y = c(rep(NA, (tlen-n_ahead)), mlp_pred$mean)), col = 'red') +
  #  ggtitle(paste('MLP', n_ahead, 'Month ASE: ', mlp_ase), subtitle =paste(target_df$Product[1], 'Purchases By Cust ID: ', target_df$Customer_ID[1]))
  
  #if(display == TRUE){
  #  print(fit_mlp)
  #  plot(fit_mlp)
  #  show(r1)}
  
  forecasts <- data.frame(forecast = mlp_pred$mean)
  return(c(forecasts, mlp_ase))
}

#function that accepts function for collated rolling ase 1:12 based on trainingSize ect, so i can just swap ar_ts with rf_ts and reuse the same evaluation matrix 
ase_gen <- function(target_df, FUNCTION){
  #FUNCTION = ar_ts
  j = 12 #n ahead
  ntotal <- length(target_df$STD_Cases)#+1
  trainingSize = round(ntotal*.7)
  #60 #will cause problems with the shorter datasets if left at flat 60
  temp_roll_ase = c() #holder
  temp_ase_matrix = matrix(0, ncol = 12, nrow = length(1:(ntotal-(trainingSize + j) + 1))) #preallocation
  
  for(k in 1:(ntotal-(trainingSize + j) + 1)){
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
      temp_forecasts <- FUNCTION(j, target_df[k:(k+(trainingSize-1)), ])#feed ar_ts dataframe with shifting rows
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
#have to sink/closeallconnections the function to hide tswge table output


