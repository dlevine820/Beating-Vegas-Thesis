set.seed(1234)
#Perform 10 fold cross validation
lm_error <- c()
lmer_error <- c()
money <- c()
total_bets <- c()
bankroll <- c()
bankroll2 <- c()
bankroll3 <- c()
bankroll4 <- c()
bankroll5 <- c()
bankroll6 <- c()
bankroll_agree_final <- c()
bankroll_martin_final <- c()
bankroll_extreme_final <- c()
kelly_perc <- c()
tracker1 <- c()
tracker2 <- c()
tracker3 <- c()
tracker4 <- c()
tracker5 <- c()
tracker6 <- c()
tracker_agree <- c()
tracker_martin <- c()
tracker_extreme <- c()
blmer_error <- c()
lmer2_error <- c()

for(i in 1:100){
  #Segement your data by fold using the which() function 
  more_stat4 <- more_stat3[sample(nrow(more_stat3)),]
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(more_stat4)),breaks=7,labels=FALSE)
  testIndexes <- which(folds==1,arr.ind=TRUE)
  testData <- more_stat4[testIndexes, ]
  trainData <- more_stat4[-testIndexes, ]
  first_reg <- lm(data = trainData, score_diff_away ~ poly(home.wins, 2) +
                    poly(away.wins,2) + (first_decision_point_spread)+ home.winpercent * poly(home.wins, 2) + 
                    away.winpercent * poly(away.wins,2) + away_WEI.dvoa + home_WEI.dvoa + neutral_venue)
  
  lmer_reg <- lmer(data = trainData, 
                        score_diff_away ~ (1|away_team) +  poly(home.wins, 2) +
                          poly(away.wins,2) + (first_decision_point_spread)+ home.winpercent * poly(home.wins, 2) + 
                          away.winpercent * poly(away.wins,2) + away_WEI.dvoa)
  
  lmer_reg2 <- lmer(data = trainData, score_diff_away ~ (1|away_team) + 
                           (1|Year) +
                           log_away_cash_bet + log_home_cash_bet + log_away_tic_num + log_home_tic_num + 
                           WEI_away.diff
                         + (first_decision_point_spread) + away_total.dvoa + cash_tic_diff)
  
  predictions_simple <- predict(first_reg, testData)
  lmer_pred_simple <- predict(lmer_reg, testData)
  lmer2_pred_simple <- predict(lmer_reg2, testData)
  
  actual <- testData$away_score - testData$home_score
  
  #find error rates in scores
  lm_error <- append(lm_error, (sum(predictions_simple - actual) / nrow(testData)))
  lmer_error <- append(lmer_error, (sum(lmer_pred_simple - actual) / nrow(testData)))
  lmer2_error <- append(lmer2_error, (sum(lmer2_pred_simple - actual) / nrow(testData)))
  
  
  #SIMPLE BETTING STRATEGY
  
  #calculate amount money won for each set of test data if we bet 10 units each time
  
  se_predict <- predict(first_reg, testData, interval = "prediction", se.fit=TRUE)$residual.scale
  spread_diff <- predictions_simple + testData$away_line_close
  df_predict <- predict(first_reg, testData, interval = "prediction", se.fit=TRUE)$df
  pred_probs_simple <- pt(spread_diff / se_predict, df = df_predict)
  
  #PLACING BETS
  
  sim_bets_away <- testData$away_team_cover[which((pred_probs_simple) > ((1-pred_probs_simple)*1.1))]
  sim_bets_home <- testData$away_team_cover[which((1-pred_probs_simple) > (pred_probs_simple*1.1))]
  
  money_away <- 10*sum(sim_bets_away) - (length(sim_bets_away) - sum(sim_bets_away))*11
  money_home <- 10*(length(sim_bets_home) - sum(sim_bets_home)) - (sum(sim_bets_home)*11)
  total_won <- money_home + money_away
  bets <- length(sim_bets_away) + length(sim_bets_home)
  total_bets <- append(total_bets, bets)
  money <- append(money, total_won)
  
  
  #SIMULATIONS
  
  qwd <- predictInterval(lmer_reg, testData, returnSims = TRUE, n.sims=500)
  sim.results <- attributes(qwd)$sim.results
  sim.results <- data.frame(t(sim.results))
  
  
  qwd2 <- predictInterval(lmer_reg2, testData, returnSims = TRUE, n.sims=500)
  sim.results2 <- attributes(qwd2)$sim.results
  sim.results2 <- data.frame(t(sim.results2))
  
  sim_probs <- c()
  sim_probs2 <- c()
  
  fut_bet <- c()
  is_it_low_bound <- c()
  
  decision_point_spread <- c()
  out_of_interval <- c()
  
  forecasted_new_spread <- c()
  fut_prob <- c()
  
  fut_spread_bet_number <- c()
  
  cur_sim_prob <- c()
  
  for (i in 1:nrow(testData)){
    df <- eval(parse(text = paste0("df_", testData$gameID[i], "_Week", testData$Week[i], "_",
                                   testData$Year[i])))
    forecasting_df <- eval(parse(text = paste0("forecast_", testData$gameID[i], "_Week", testData$Week[i], "_",
                                               testData$Year[i])))
    
    h <- round(nrow(df) / 3)
    df_for_decision <- df[1:(nrow(df) - h + 1),]
    df_after_decision <- df[(nrow(df) - h):nrow(df),]
    
    decision_point_spread[i] <- as.numeric(tail(df_for_decision$spread, n=1))
    
    sim_probs[i] = ecdf(sim.results[,i])(-decision_point_spread[i])
    sim_probs2[i] = ecdf(sim.results2[,i])(-decision_point_spread[i])
    
    cur_sim_prob[i] = ifelse(sim_probs2[i] < .5, (1 - ecdf(sim.results2[,i])(-decision_point_spread[i])), 
                             ecdf(sim.results2[,i])(-decision_point_spread))
    
    outside_of_interval <- c()
    for (j in 1:nrow(forecasting_df)){
      outside_of_interval[j] <- ifelse( (decision_point_spread[i] > forecasting_df[j,]$high_bound_round | 
                                           decision_point_spread[i] < forecasting_df[j,]$low_bound_round) 
                                        & decision_point_spread[i] != forecasting_df[j,]$rounded_spread, forecasting_df[j,]$rounded_spread, 0)
    }
    difference_in_spreads <- ifelse(outside_of_interval == 0, outside_of_interval, 
                                    abs(decision_point_spread[i] - outside_of_interval))
    
    forecasted_new_spread[i] <- outside_of_interval[which(difference_in_spreads == max(difference_in_spreads))][1]
    out_of_interval[i] <- ifelse(sum(outside_of_interval) != 0, 1,0)
    
    fut_prob[i] = case_when(
      forecasted_new_spread[i] != 0 & sim_probs2[i] < .5 & out_of_interval[i] == 1 ~ 
        ((1 - ecdf(sim.results2[,i])(-forecasted_new_spread[i])) * 0.8),
      forecasted_new_spread[i] != 0 & sim_probs2[i] >= .5 & out_of_interval[i] == 1 ~ 
        ecdf(sim.results2[,i])(-forecasted_new_spread[i]) * 0.8,
      forecasted_new_spread[i] == 0 | out_of_interval[i] == 0 ~ cur_sim_prob[i])
    
    fut_bet[i] = 
      case_when(cur_sim_prob[i] >= fut_prob[i] | out_of_interval[i] == 0 ~ 0,
                cur_sim_prob[i] < fut_prob[i] & out_of_interval[i] == 1 ~ 1)
    
    is_it_low_bound[i] = ifelse(forecasted_new_spread[i] > decision_point_spread[i] & fut_bet[i] == 1, 1, 0)
    
    after_decision_spread <- as.numeric(df_after_decision$spread)
    forecast_decis_diff <- forecasted_new_spread[i] - decision_point_spread[i]
    forecast_true_diff <- forecasted_new_spread[i] - after_decision_spread
    
    fut_spread_bet_number[i] <- after_decision_spread[which(forecast_decis_diff < forecast_true_diff)[1]]
  }
  
  #betting strategy depends on expected value -- using Kelly criterion system
  #the expected value is the proportion of bankroll that should be bet on each game
  #assume $100 bankroll
  #some expected values too high; cap maximum bet at 20% of bankroll
  
  #in addition; while long run probability gets to nearly infinity with this strategy, in short term
  #too easy to lose money
  #Capping absolute maximum bet at 40
  max_bet <- 40
  
  #using simulated probabilities and simple lm prediction probabilities
  
  
  testData$sim_probs <- sim_probs
  testData$sim_probs2 <- sim_probs2
  testData$decision_point_spread <- decision_point_spread
  testData$forecasted_new_spread <- forecasted_new_spread
  testData$fut_prob <- fut_prob
  testData$fut_bet <- fut_bet
  testData$is_it_low_bound <- is_it_low_bound
  testData$home_minus_away_score <- testData$home_score - testData$away_score
  
  
  
  testData$away_team_cover_cur = 
    case_when(decision_point_spread > testData$home_minus_away_score ~ 1,
              decision_point_spread == testData$home_minus_away_score ~ -1,
              decision_point_spread < testData$home_minus_away_score ~ 0)
  
  testData$away_team_cover_fut = 
    case_when(fut_spread_bet_number > testData$home_minus_away_score ~ 1,
              fut_spread_bet_number == testData$home_minus_away_score ~ -1,
              fut_spread_bet_number < testData$home_minus_away_score ~ 0)
  
  testData2 <- testData %>% mutate(exp_value =
                                     case_when(sim_probs < .5 ~ ((1 - sim_probs) - (sim_probs * 1.1)),
                                               sim_probs >= .5 ~ ((sim_probs) - ((1-sim_probs) * 1.1))),
                                   bet_team = 
                                     case_when(sim_probs < .5 ~ 1,
                                               sim_probs >= .5 ~ 0),
                                   bet_win =
                                     case_when(bet_team == away_team_cover_cur & away_team_cover_cur != -1 ~ 1,
                                               bet_team != away_team_cover_cur & away_team_cover_cur != -1 ~ 0,
                                               away_team_cover_cur == -1 ~ -1)
  )
  
  
  testData2$sim_probs2 <- sim_probs2
  testData2 <- testData2 %>% mutate(exp_value2 =
                                      case_when(sim_probs2 < .5 ~ ((1 - sim_probs2) - (sim_probs2 * 1.1)),
                                                sim_probs2 >= .5 ~ ((sim_probs2) - ((1-sim_probs2) * 1.1))),
                                    bet_team2 = 
                                      case_when(sim_probs2 < .5 ~ 1,
                                                sim_probs2 >= .5 ~ 0),
                                    bet_win2 =
                                      case_when(bet_team2 == away_team_cover_cur & away_team_cover_cur != -1 ~ 1,
                                                bet_team2 != away_team_cover_cur & away_team_cover_cur != -1 ~ 0,
                                                away_team_cover_cur == -1 ~ -1)
  )
  
  testData2$pred_probs_simple <- pred_probs_simple
  testData2 <- testData2 %>% mutate(exp_value3 =
                                      case_when(pred_probs_simple < .5 ~ ((1 - pred_probs_simple) - (pred_probs_simple * 1.1)),
                                                pred_probs_simple >= .5 ~ ((pred_probs_simple) - ((1-pred_probs_simple) * 1.1))),
                                    bet_team3 = 
                                      case_when(pred_probs_simple < .5 ~ 1,
                                                pred_probs_simple >= .5 ~ 0),
                                    bet_win3 =
                                      case_when(bet_team3 == away_team_cover_cur & away_team_cover_cur != -1 ~ 1,
                                                bet_team3 != away_team_cover_cur & away_team_cover_cur != -1 ~ 0,
                                                away_team_cover_cur == -1 ~ -1)
  )
  
  testData2 <- testData2 %>% mutate(fut_exp_value =
                                      case_when(fut_prob < .5 ~ ((1 - fut_prob) - (fut_prob * 1.1)),
                                                fut_prob >= .5 ~ ((fut_prob) - ((1-fut_prob) * 1.1))),
                                    fut_bet_team = 
                                      case_when(fut_prob < .5 ~ 1,
                                                fut_prob >= .5 ~ 0),
                                    fut_bet_win =
                                      case_when(fut_bet_team == away_team_cover_fut & away_team_cover_fut != -1 ~ 1,
                                                fut_bet_team != away_team_cover_fut & away_team_cover_fut != -1 ~ 0,
                                                away_team_cover_fut == -1 ~ -1)
  )
  
  testData2$exp_value_capped <- ifelse(testData2$exp_value > .2, .2, testData2$exp_value)
  testData2$exp_value_capped2 <- ifelse(testData2$exp_value2 > .2, .2, testData2$exp_value2)
  testData2$exp_value_capped3 <- ifelse(testData2$exp_value3 > .2, .2, testData2$exp_value3)
  testData2$exp_value_transformed <- ifelse(testData2$exp_value >= 0, testData2$exp_value^2, -testData2$exp_value^2)
  testData2$exp_value2_transformed <- ifelse(testData2$exp_value2 >= 0, testData2$exp_value2^2, -testData2$exp_value2^2)
  
  testData2$exp_value_agree <- ifelse(testData2$bet_team == testData2$bet_team2, 
                                      (testData2$exp_value + testData2$exp_value2) / 2, 0)
  
  testData2$exp_value_agree_capped <- ifelse(testData2$exp_value_agree > .2, .2, testData2$exp_value_agree)
  
  
  bankroll_current <- 100
  bankroll_current2 <- 100
  bankroll_current3 <- 100
  bankroll_current4 <- 100
  bankroll_current5 <- 100
  bankroll_current6 <- 100
  bankroll_extreme <- 100
  bankroll_martin <- 100
  bankroll_agree <- 100
  
  for (i in 1:nrow(testData2)){
    
    bet_amt_cur <-  case_when(testData2$exp_value_capped[i] > 0 & fut_bet[i] == 0 ~ testData2$exp_value_capped[i] * bankroll_current,
                              testData2$exp_value_capped[i] > 0 & fut_bet[i] == 1 ~ testData2$exp_value_capped[i] * bankroll_current * (1/3),
                              testData2$exp_value_capped[i] <= 0 ~ 0)
    
    
    
    bet_amt_cur <- ifelse(bet_amt_cur > max_bet, max_bet, bet_amt_cur)
    
    amt_won <- case_when(
      testData2$bet_win[i] == 1 ~ (bet_amt_cur / 1.1),
      testData2$bet_win[i] == 0 ~ -bet_amt_cur,
      testData2$bet_win[i] == -1 ~ 0
    )
    
    bankroll_current = bankroll_current + amt_won
    tracker1 <- append(tracker1, bankroll_current)
    
    bet_amt_cur2 <-  case_when(testData2$exp_value_capped2[i] > 0 & fut_bet[i] == 0 ~ testData2$exp_value_capped2[i] * bankroll_current2,
                               testData2$exp_value_capped2[i] > 0 & fut_bet[i] == 1 ~ testData2$exp_value_capped2[i] * bankroll_current2 *(1/3),
                               testData2$exp_value_capped2[i] <= 0 ~ 0)
    
    
    
    bet_amt_cur2 <- ifelse(bet_amt_cur2 > max_bet, max_bet, bet_amt_cur2)
    
    amt_won2 <- case_when(
      testData2$bet_win2[i] == 1 ~ (bet_amt_cur2 / 1.1),
      testData2$bet_win2[i] == 0 ~ -bet_amt_cur2,
      testData2$bet_win2[i] == -1 ~ 0
    )
    
    bankroll_current2 = bankroll_current2 + amt_won2
    tracker2 <- append(tracker2, bankroll_current2)
    
    
    bet_amt_cur3 <- case_when(testData2$exp_value_capped3[i] > 0 & fut_bet[i] == 0 ~ testData2$exp_value_capped3[i] * bankroll_current3,
                              testData2$exp_value_capped3[i] > 0 & fut_bet[i] == 1 
                              ~ testData2$exp_value_capped3[i] * bankroll_current3 * (1/3),
                              testData2$exp_value_capped3[i] <= 0 ~ 0)
    
    
    bet_amt_cur3 <- ifelse(bet_amt_cur3 > max_bet, max_bet, bet_amt_cur3)
    
    amt_won3 <- case_when(
      testData2$bet_win3[i] == 1 ~ (bet_amt_cur3 / 1.1),
      testData2$bet_win3[i] == 0 ~ -bet_amt_cur3,
      testData2$bet_win3[i] == -1 ~ 0
    )
    
    bankroll_current3 = bankroll_current3 + amt_won3
    tracker3 <- append(tracker3, bankroll_current3)
    
    
    bet_amt_cur4 <- case_when(testData2$exp_value_transformed[i] > 0 & fut_bet[i] == 0 
                              ~ testData2$exp_value_transformed[i] * bankroll_current4,
                              testData2$exp_value_transformed[i] > 0 & fut_bet[i] == 1 
                              ~ testData2$exp_value_transformed[i] * bankroll_current4 * (1/3),
                              testData2$exp_value_transformed[i] <= 0 ~ 0)
    
    amt_won4 <- case_when(
      testData2$bet_win[i] == 1 ~ (bet_amt_cur4 / 1.1),
      testData2$bet_win[i] == 0 ~ -bet_amt_cur4,
      testData2$bet_win[i] == -1 ~ 0
    )
    
    bankroll_current4 = bankroll_current4 + amt_won4
    tracker4 <- append(tracker4, bankroll_current4)
    
    
    bet_amt_cur5 <-  case_when(testData2$exp_value_capped[i] > 0 ~ 15,
                               testData2$exp_value_capped[i] <= 0 ~ 0)
    
    amt_won5 <- case_when(
      testData2$bet_win[i] == 1 ~ (bet_amt_cur2 / 1.1),
      testData2$bet_win[i] == 0 ~ -bet_amt_cur2,
      testData2$bet_win[i] == -1 ~ 0
    )
    
    bankroll_current5 = bankroll_current5 + amt_won5
    tracker5 <- append(tracker5, bankroll_current5)
    
    bet_amt_cur_martin <-  case_when(testData2$exp_value_capped[i] <= 0 ~ 0,
                                     testData2$exp_value_capped[i] > 0 & bankroll_martin >= 100 ~ 5,
                                     testData2$exp_value_capped[i] > 0 & bankroll_martin < 100 ~ ((105-bankroll_martin)*1.1))
    
    bet_amt_cur_martin <- ifelse(bet_amt_cur_martin > bankroll_martin, bankroll_martin, bet_amt_cur_martin)
    bet_amt_cur_martin <- ifelse(bankroll_martin <= 0, 0, bet_amt_cur_martin)
    
    amt_won_martin <- case_when(
      testData2$bet_win[i] == 1 ~ (bet_amt_cur_martin / 1.1),
      testData2$bet_win[i] == 0 ~ -bet_amt_cur_martin,
      testData2$bet_win[i] == -1 ~ 0
    )
    
    bankroll_martin = bankroll_martin + amt_won_martin
    tracker_martin <- append(tracker_martin, bankroll_martin)
    
    bet_amt_cur_extreme <-  case_when(testData2$exp_value[i] <= 0.1 ~ 0,
                                      testData2$exp_value[i] > 0.1 & testData2$exp_value[i] <= 0.2 ~ 10,
                                      testData2$exp_value[i] > 0.2 ~ 20)
    
    bet_amt_cur_extreme <- ifelse(bet_amt_cur_extreme > bankroll_extreme, bankroll_extreme, bet_amt_cur_extreme)
    bet_amt_cur_extreme <- ifelse(bankroll_extreme <= 0, 0, bet_amt_cur_extreme)                    
    
    amt_won_extreme <- case_when(
      testData2$bet_win[i] == 1 ~ (bet_amt_cur_extreme / 1.1),
      testData2$bet_win[i] == 0 ~ -bet_amt_cur_extreme,
      testData2$bet_win[i] == -1 ~ 0
    )
    
    bankroll_extreme = bankroll_extreme + amt_won_extreme
    tracker_extreme <- append(tracker_extreme, bankroll_extreme)
    
    
    bet_amt_cur6 <- case_when(testData2$exp_value[i] <= 0.1 ~ 0,
                              testData2$exp_value[i] > 0.1 & testData2$fut_bet[i] == 0 ~ 
                                testData2$exp_value_transformed[i] * bankroll_current6,
                              testData2$exp_value[i] > 0.1 & testData2$fut_bet[i] == 1 ~ 
                                testData2$exp_value_transformed[i] * bankroll_current6 * (1/3) )
    
    amt_won6 <- case_when(
      testData2$bet_win[i] == 1 ~ (bet_amt_cur6 / 1.1),
      testData2$bet_win[i] == 0 ~ -bet_amt_cur6,
      testData2$bet_win[i] == -1 ~ 0
    )
    
    bankroll_current6 = bankroll_current6 + amt_won6
    tracker6 <- append(tracker6, bankroll_current6)
    
    bet_amt_cur_agree <- case_when(testData2$exp_value_agree_capped[i] > 0 ~
                                     testData2$exp_value_agree_capped[i] * bankroll_agree,
                                   testData2$exp_value_agree_capped[i] <= 0  ~ 0)
    
    amt_won6 <- case_when(
      testData2$bet_win2[i] == 1 ~ (bet_amt_cur_agree / 1.1),
      testData2$bet_win2[i] == 0 ~ -bet_amt_cur_agree,
      testData2$bet_win2[i] == -1 ~ 0
    )
    
    bankroll_agree = bankroll_agree + amt_won6
    tracker_agree <- append(tracker_agree, bankroll_agree)
    
  }
  
  for (i in 1:nrow(testData2)){
    bet_amt_fut <- case_when(testData2$exp_value_capped[i] > 0 & fut_bet[i] == 1 & !is.na(fut_spread_bet_number[i]) ~
                               testData2$fut_exp_value[i] * bankroll_current * (2/3),
                             
                             testData2$exp_value_capped[i] <= 0 & fut_bet[i] == 1 & !is.na(fut_spread_bet_number[i]) ~
                               testData2$fut_exp_value[i] * bankroll_current,
                             
                             fut_bet[i] == 0 | is.na(fut_spread_bet_number[i]) ~ 0)
    
    bet_amt_fut <- ifelse(bet_amt_fut > max_bet, max_bet, bet_amt_fut)
    
    amt_won_fut <- case_when(
      testData2$fut_bet_win[i] == 1 ~ (bet_amt_fut / 1.1),
      testData2$fut_bet_win[i] == 0 ~ -bet_amt_fut,
      testData2$fut_bet_win[i] == -1 ~ 0,
      is.na(testData2$fut_bet_win[i]) ~ 0
      
    )
    
    bankroll_current = bankroll_current + amt_won_fut
    tracker1 <- append(tracker1, bankroll_current)
    
    
    bet_amt_fut2 <- case_when(testData2$exp_value_capped2[i] > 0 & fut_bet[i] == 1 & !is.na(fut_spread_bet_number[i]) ~
                                testData2$fut_exp_value[i] * bankroll_current2 * (2/3),
                              
                              testData2$exp_value_capped2[i] <= 0 & fut_bet[i] == 1 & !is.na(fut_spread_bet_number[i]) ~
                                testData2$fut_exp_value[i] * bankroll_current2,
                              
                              fut_bet[i] == 0 | is.na(fut_spread_bet_number[i]) ~ 0)
    
    amt_won_fut2 <- case_when(
      testData2$fut_bet_win[i] == 1 ~ (bet_amt_fut2 / 1.1),
      testData2$fut_bet_win[i] == 0 ~ -bet_amt_fut2,
      testData2$fut_bet_win[i] == -1 ~ 0,
      is.na(testData2$fut_bet_win[i]) ~ 0
    )
    
    bankroll_current2 = bankroll_current2 + amt_won_fut2
    tracker2 <- append(tracker2, bankroll_current2)
    
    bet_amt_fut3 <- case_when(testData2$exp_value_capped3[i] > 0 & fut_bet[i] == 1 & !is.na(fut_spread_bet_number[i]) ~
                                testData2$fut_exp_value[i] * bankroll_current3 * (2/3),
                              
                              testData2$exp_value_capped3[i] <= 0 & fut_bet[i] == 1 & !is.na(fut_spread_bet_number[i]) ~
                                testData2$fut_exp_value[i] * bankroll_current3,
                              
                              fut_bet[i] == 0 | is.na(fut_spread_bet_number[i]) ~ 0)
    
    amt_won_fut3 <- case_when(
      testData2$fut_bet_win[i] == 1 ~ (bet_amt_fut3 / 1.1),
      testData2$fut_bet_win[i] == 0 ~ -bet_amt_fut3,
      testData2$fut_bet_win[i] == -1 ~ 0,
      is.na(testData2$fut_bet_win[i]) ~ 0
      
    )
    
    bankroll_current3 = bankroll_current3 + amt_won_fut3
    tracker3 <- append(tracker3, bankroll_current3)
    
    bet_amt_fut4 <- case_when(testData2$exp_value_transformed[i] > 0 & fut_bet[i] == 1 & !is.na(fut_spread_bet_number[i]) ~
                                testData2$fut_exp_value[i] * bankroll_current4 * (2/3),
                              
                              testData2$exp_value_transformed[i] <= 0 & fut_bet[i] == 1 & !is.na(fut_spread_bet_number[i]) ~
                                testData2$fut_exp_value[i] * bankroll_current4,
                              
                              fut_bet[i] == 0 | is.na(fut_spread_bet_number[i]) ~ 0)
    
    amt_won_fut4 <- case_when(
      testData2$fut_bet_win[i] == 1 ~ (bet_amt_fut4 / 1.1),
      testData2$fut_bet_win[i] == 0 ~ -bet_amt_fut4,
      testData2$fut_bet_win[i] == -1 ~ 0,
      is.na(testData2$fut_bet_win[i]) ~ 0
      
    )
    
    bankroll_current4 = bankroll_current4 + amt_won_fut4
    tracker4 <- append(tracker4, bankroll_current4)
    
    bet_amt_fut6 <- case_when(testData2$exp_value[i] > 0.1 & fut_bet[i] == 1 & !is.na(fut_spread_bet_number[i]) ~
                                testData2$fut_exp_value[i] * testData2$fut_exp_value[i] * bankroll_current6 * (2/3),
                              testData2$exp_value_transformed[i] <= 0.1 & testData2$fut_exp_value[i] > .1 & fut_bet[i] == 1 &  
                                !is.na(fut_spread_bet_number[i]) ~
                                testData2$fut_exp_value[i] * testData2$fut_exp_value[i] * bankroll_current6,
                              fut_bet[i] == 0 | is.na(fut_spread_bet_number[i]) | testData2$fut_exp_value[i] <= 0.1 ~ 0)
    
    amt_won_fut6 <- case_when(
      testData2$fut_bet_win[i] == 1 ~ (bet_amt_fut6 / 1.1),
      testData2$fut_bet_win[i] == 0 ~ -bet_amt_fut6,
      testData2$fut_bet_win[i] == -1 ~ 0,
      is.na(testData2$fut_bet_win[i]) ~ 0
      
    )
    
    bankroll_current6 = bankroll_current6 + amt_won_fut6
    tracker6 <- append(tracker6, bankroll_current6)
    
  }
  
  bankroll <- append(bankroll, bankroll_current)
  bankroll2 <- append(bankroll2, bankroll_current2)
  bankroll3 <- append(bankroll3, bankroll_current3)
  bankroll4 <- append(bankroll4, bankroll_current4)
  bankroll5 <- append(bankroll5, bankroll_current5)
  bankroll6 <- append(bankroll6, bankroll_current6)
  bankroll_martin_final <- append(bankroll_martin_final, bankroll_martin)
  bankroll_extreme_final <- append(bankroll_extreme_final, bankroll_extreme)
  bankroll_agree_final <- append(bankroll_agree_final, bankroll_agree)
}



bankroll.df <- data.frame(cbind(bankroll, bankroll2, bankroll3, bankroll4, 
                                bankroll5, bankroll6, bankroll_martin_final, bankroll_extreme_final, bankroll_agree_final))

colnames(bankroll.df) <- c("K1", "K2", "K3", "K1-Transform", "Simple Bets", "Extreme Edge - Transformed", "Martingale", "Extreme Edge - Simple", "LMER Agreement")

tracker.df <- data.frame(cbind(tracker1, tracker2, tracker3, tracker4, tracker5, tracker6, tracker_agree,
                              tracker_extreme, tracker_martin))

colnames(tracker.df) <- c("K1", "K2", "K3", "K1-Transform", "Simple Bets", "Extreme Edge - Transformed", "LMER Agreement", "Extreme Edge - Simple", "Martingale")

write.csv(bankroll.df, "bankroll_df.csv", row.names = F)
write.csv(tracker.df, "tracker_df.csv", row.names = F)
