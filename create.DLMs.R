#DLM
#dlm aspect

buildTVP <- function(parm, x.mat){
  parm <- exp(parm)
  return(dlmModReg(X=x.mat, dV=parm[1], dW=c(parm[2], parm[3])) )
}

error_rate <- c()
error_rate2 <- c()
for (i in 1:nrow(nfl_scores2018)){
  gbg_df <- eval(parse(text = paste0("df_", list_of_games[i], "_Week", nfl_scores2018$Week[i], "_",
                                     nfl_scores2018$Year[i])))
  gbg_df <- gbg_df[2:nrow(gbg_df),]
  gbg_df$spread <- as.numeric(gbg_df$spread)
  h <- round(nrow(gbg_df) / 3)
  true_final_spreads <- as.numeric(gbg_df$spread[(nrow(gbg_df) - h + 1):nrow(gbg_df)])
  df_for_forecasting <- gbg_df[1:(nrow(gbg_df) - h),]
  
  newspread <- c(rep(NA, h))
  
  newlogawaycash <- forecast(auto.arima(df_for_forecasting$log_away_cash_bet, seasonal = FALSE), h = h)$mean
  newloghomecash <- forecast(auto.arima(df_for_forecasting$log_home_cash_bet, seasonal = FALSE), h = h)$mean
  newlogawaytic <- forecast(auto.arima(df_for_forecasting$log_away_tic_num, seasonal = FALSE), h = h)$mean
  newloghometic <- forecast(auto.arima(df_for_forecasting$log_home_tic_num, seasonal = FALSE), h = h)$mean
  
  newawaycashpercent <- exp(newlogawaycash) / (exp(newlogawaycash) + exp(newloghomecash))
  newawayticpercent <- exp(newlogawaytic) / (exp(newlogawaytic) + exp(newloghometic))
  
  newhometic <- exp(newloghometic)
  newawaytic <- exp(newlogawaytic)
  
  newhomecash <- exp(newloghomecash)
  newawaycash <- exp(newlogawaycash)
  
  
  
  newdata <- data.frame(newspread, newlogawaycash, newloghomecash, newlogawaytic, 
                        newloghometic, newawaycashpercent, newawayticpercent, newawaytic, newhometic,
                        newawaycash, newhomecash)
  colnames(newdata) <- c("spread", "log_away_cash_bet", "log_home_cash_bet", "log_away_tic_num", "log_home_tic_num",
                         "away_cash_percent", "away_ticket_percent", "away_ticket_num", "home_ticket_num",
                         "away_cash_bet", "home_cash_bet")
  
  filter_df <- rbind.fill(df_for_forecasting, newdata)
  
  start.vals = c(var(df_for_forecasting$spread), 0,0)
  
  xreg_arima = matrix(data = c(df_for_forecasting$log_away_cash_bet, df_for_forecasting$away_ticket_num,
                               df_for_forecasting$log_home_cash_bet, df_for_forecasting$home_ticket_num), nrow = nrow(df_for_forecasting), ncol = 4)
  
  arima.model <- auto.arima(df_for_forecasting$spread, seasonal = FALSE, 
                            xreg = xreg_arima)
  
  newxreg_arima = matrix(data = c(newdata$log_away_cash_bet, newdata$away_ticket_num,
                                  newdata$log_home_cash_bet, newdata$home_ticket_num), nrow = nrow(newdata), ncol = 4)
  
  arima_forecasts = forecast(arima.model, xreg = newxreg_arima, h = h)
  
  TVP.mle <- dlmMLE(y=filter_df$spread, parm=start.vals, x.mat= filter_df$log_away_cash_bet +
                      filter_df$away_ticket_num
                    + filter_df$log_home_cash_bet + filter_df$home_ticket_num,
                    build=buildTVP, method="BFGS")
  
  TVP.dlm <- buildTVP(TVP.mle$par, x.mat = filter_df$log_away_cash_bet +
                        filter_df$away_ticket_num
                      + filter_df$log_home_cash_bet + filter_df$home_ticket_num)
  
  TVP.f <- dlmFilter(y = filter_df$spread, mod = TVP.dlm)
  
  v <- sapply((nrow(gbg_df) - h + 1):nrow(gbg_df), function(x) (dlmSvd2var(TVP.f$U.R, TVP.f$D.R)[[x]][1]))
  
  low_bound <- TVP.f$f[(nrow(filter_df) - h + 1):nrow(filter_df)] + qnorm(0.10, sd = sqrt(abs(v)))
  high_bound <- TVP.f$f[(nrow(filter_df) - h + 1):nrow(filter_df)] + qnorm(0.90, sd = sqrt(abs(v)))
  
  newdata$spread <- TVP.f$f[(nrow(filter_df) - h + 1):nrow(filter_df)]
  newdata$rounded_spread <- round(newdata$spread / .5) * .5
  
  newdata$low_bound <- low_bound
  newdata$high_bound <- high_bound
  
  newdata$low_bound_round <- round(low_bound / .5) * .5
  newdata$high_bound_round <- round(high_bound / .5) * .5
  
  newdata$spread_arima <- arima_forecasts$mean
  newdata$rounded_spread_arima <- round(newdata$spread_arima / .5) * .5
  
  newdata$low_bound_arima <- arima_forecasts$lower[,1]
  newdata$high_bound_arima <- arima_forecasts$upper[,2]
  
  newdata$low_bound_round_arima <- round(newdata$low_bound_arima / .5) * .5
  newdata$high_bound_round_arima <- round(newdata$high_bound_arima / .5) * .5
  
  newdata$low_bound_round <- round(low_bound / .5) * .5
  newdata$high_bound_round <- round(high_bound / .5) * .5
  
  error <- newdata$spread - true_final_spreads
  error_rate <- append(error_rate, mean(error))
  
  error2 <- newdata$spread_arima - true_final_spreads
  error_rate2 <- append(error_rate2, mean(error2))
  
  assign(paste0("forecast_", list_of_games[i], "_Week", nfl_scores2018$Week[i], "_",
                nfl_scores2018$Year[i]), newdata)
  
}

