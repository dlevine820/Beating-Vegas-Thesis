for (week in c(3:17)){
  true_week <- week - 1
  url <- paste0("https://www.footballoutsiders.com/dvoa-ratings/2017/week-", true_week, "-dvoa-ratings")
  webpage <- read_html(url)
  dvoa_nodes <- html_nodes(webpage, "table.stats")
  dvoa_tables <- html_table(dvoa_nodes, header = TRUE)
  if (length(dvoa_tables) > 1) {
    p <- sapply(dvoa_tables, function(e) ncol(as.data.frame(e)))
    dvoa <- html_table(dvoa_nodes, header = TRUE)[which(p == 13 | p ==12)[1]]
  }
  else {
    dvoa <- html_table(dvoa_nodes, header = TRUE)
  }
  dvoa_table <- data.frame(dvoa)
  dvoa_table$TEAM <- gsub("LACH", "LAC", dvoa_table$TEAM)
  dvoa_table$TEAM <- gsub("LARM", "LAR", dvoa_table$TEAM)
  dvoa_table$TEAM <- gsub("JAX", "JAC", dvoa_table$TEAM)
  assign(paste0("DVOA_Week", week, "_2017"), dvoa_table)
}

#Week 4 2017
url_week4_2018 <- paste0("https://www.footballoutsiders.com/dvoa-ratings/2018/week-", 3, "-dvoa-ratings")
webpage_week4_2018 <- read_html(url_week4_2018)
dvoa_nodes_week4_2018 <- html_nodes(webpage_week4_2018, "table.stats")
dvoa_table_week4_2018 <- html_table(dvoa_nodes_week4_2018, header = TRUE)[3]
dvoa_table_week4_2018 <- data.frame(dvoa_table_week4_2018)
dvoa_table_week4_2018$TEAM <- gsub("LACH", "LAC", dvoa_table_week4_2018$TEAM)
dvoa_table_week4_2018$TEAM <- gsub("LARM", "LAR", dvoa_table_week4_2018$TEAM)
dvoa_table_week4_2018$TEAM <- gsub("JAX", "JAC", dvoa_table_week4_2018$TEAM)
DVOA_Week4_2018 <- dvoa_table_week4_2018


url_week2_2018 <- paste0("https://www.footballoutsiders.com/dvoa-ratings/2018/week-", 1, "-dvoa-ratings")
webpage_week2_2018 <- read_html(url_week2_2018)
dvoa_nodes_week2_2018 <- html_nodes(webpage_week2_2018, "table.stats")
dvoa_table_week2_2018 <- html_table(dvoa_nodes_week2_2018, header = TRUE)[2]
dvoa_table_week2_2018 <- data.frame(dvoa_table_week2_2018)
dvoa_table_week2_2018$last_week <- rep(0, nrow(dvoa_table_week2_2018))
dvoa_table_week2_2018 <- dvoa_table_week2_2018[, c(1:3, 13, 4:12)]
dvoa_table_week2_2018$TEAM <- gsub("LACH", "LAC", dvoa_table_week2_2018$TEAM)
dvoa_table_week2_2018$TEAM <- gsub("LARM", "LAR", dvoa_table_week2_2018$TEAM)
dvoa_table_week2_2018$TEAM <- gsub("JAX", "JAC", dvoa_table_week2_2018$TEAM)
DVOA_Week2_2018 <- dvoa_table_week2_2018

url_week2_2017 <- paste0("https://www.footballoutsiders.com/dvoa-ratings/2017/week-", 1, "-dvoa-ratings")
webpage_week2_2017 <- read_html(url_week2_2017)
dvoa_nodes_week2_2017 <- html_nodes(webpage_week2_2017, "table.stats")
dvoa_table_week2_2017 <- html_table(dvoa_nodes_week2_2017, header = TRUE)[2]
dvoa_table_week2_2017 <- data.frame(dvoa_table_week2_2017)
dvoa_table_week2_2017$last_week <- rep(0, nrow(dvoa_table_week2_2017))
dvoa_table_week2_2017 <- dvoa_table_week2_2017[, c(1:3, 13, 4:12)]
dvoa_table_week2_2017$TEAM <- gsub("LACH", "LAC", dvoa_table_week2_2017$TEAM)
dvoa_table_week2_2017$TEAM <- gsub("LARM", "LAR", dvoa_table_week2_2017$TEAM)
dvoa_table_week2_2017$TEAM <- gsub("JAX", "JAC", dvoa_table_week2_2017$TEAM)
DVOA_Week2_2017 <- dvoa_table_week2_2017

for (week in c(18:19)){
  url <- paste0("https://www.footballoutsiders.com/dvoa-ratings/2018/week-", week, "-dvoa-ratings")
  webpage <- read_html(url)
  dvoa_nodes <- html_nodes(webpage, "table.stats")
  dvoa_tables <- html_table(dvoa_nodes, header = TRUE)
  if (length(dvoa_tables) > 1) {
    p <- sapply(dvoa_tables, function(e) ncol(as.data.frame(e)))
    dvoa <- html_table(dvoa_nodes, header = TRUE)[which(p == 13)[1]]
  }
  else {
    dvoa <- html_table(dvoa_nodes, header = TRUE)
  }
  dvoa_table <- data.frame(dvoa)
  dvoa_table <- dvoa_table[,c(1:2, 12:13, 3:11)]
  dvoa_table$TEAM <- gsub("LACH", "LAC", dvoa_table$TEAM)
  dvoa_table$TEAM <- gsub("LARM", "LAR", dvoa_table$TEAM)
  dvoa_table$TEAM <- gsub("JAX", "JAC", dvoa_table$TEAM)
  assign(paste0("DVOA_Week", week, "_2017"), dvoa_table)
}

for (week in c(3, 5:17)){
  true_week <- week - 1
  url <- paste0("https://www.footballoutsiders.com/dvoa-ratings/2018/week-", true_week, "-dvoa-ratings")
  webpage <- read_html(url)
  dvoa_nodes <- html_nodes(webpage, "table.stats")
  dvoa_tables <- html_table(dvoa_nodes, header = TRUE)
  if (length(dvoa_tables) > 1) {
    p <- sapply(dvoa_tables, function(e) ncol(as.data.frame(e)))
    dvoa <- html_table(dvoa_nodes, header = TRUE)[which(p == 13 | p == 12)[1]]
  }
  else {
    dvoa <- html_table(dvoa_nodes, header = TRUE)
  }
  dvoa_table <- data.frame(dvoa)
  dvoa_table$TEAM <- gsub("LACH", "LAC", dvoa_table$TEAM)
  dvoa_table$TEAM <- gsub("LARM", "LAR", dvoa_table$TEAM)
  dvoa_table$TEAM <- gsub("JAX", "JAC", dvoa_table$TEAM)
  assign(paste0("DVOA_Week", week, "_2018"), dvoa_table)
}

for (week in c(18:19)){
  url <- paste0("https://www.footballoutsiders.com/dvoa-ratings/2019/week-", week, "-dvoa-ratings")
  webpage <- read_html(url)
  dvoa_nodes <- html_nodes(webpage, "table.sticky-headers.sortable.stats-wide")
  dvoa_tables <- html_table(dvoa_nodes, header = TRUE)
  if (length(dvoa_tables) > 1) {
    p <- sapply(dvoa_tables, function(e) ncol(as.data.frame(e)))
    dvoa <- html_table(dvoa_nodes, header = TRUE)[which(p == 13)[1]]
  }
  else {
    dvoa <- html_table(dvoa_nodes, header = TRUE)
  }
  dvoa_table <- data.frame(dvoa)
  dvoa_table <- dvoa_table[,c(1:2, 12:13, 3:11)]
  dvoa_table$TEAM <- gsub("LACH", "LAC", dvoa_table$TEAM)
  dvoa_table$TEAM <- gsub("LARM", "LAR", dvoa_table$TEAM)
  dvoa_table$TEAM <- gsub("JAX", "JAC", dvoa_table$TEAM)
  assign(paste0("DVOA_Week", week, "_2018"), dvoa_table)
}


url2016 <- "https://www.footballoutsiders.com/dvoa-ratings/2017/week-19-dvoa-ratings"
webpage2016 <- read_html(url2016)
dvoa_nodes2016 <- html_nodes(webpage2016, "table.stats")
dvoa_tables2016 <- html_table(dvoa_nodes2016, header = TRUE)
p <- sapply(dvoa_tables2016, function(e) ncol(as.data.frame(e)))
dvoa2016 <- data.frame(html_table(dvoa_nodes2016, header = TRUE)[which(p==13)[1]])
dvoa2016$TEAM <- gsub("JAX", "JAC", dvoa2016$TEAM)
dvoa2016$TEAM <- gsub("LACH", "LAC", dvoa2016$TEAM)
dvoa2016$TEAM <- gsub("LARM", "LAR", dvoa2016$TEAM)
dvoa2016 <- dvoa2016[,c(1:2, 12:13, 3:11)]


nfl_scores_with_dvoa <- data.frame(matrix(nrow = nrow(nfl_scores2018), ncol = 40))

for (i in 1:nrow(nfl_scores2018)){
  away_team <- nfl_scores2018$`Away Team`[i]
  home_team <- nfl_scores2018$`Home Team`[i]
  if (nfl_scores2018$Week[i] > 1 & nfl_scores2018$Week[i] < 17){
    dvoa_df <- eval(parse(text = paste0("DVOA_Week", 
                                        nfl_scores2018$Week[i], "_", nfl_scores2018$Year[i])))
  }
  else if (nfl_scores2018$Week[i] == 0 | nfl_scores2018$Week[i] == 17){
    dvoa_df <- eval(parse(text = paste0("DVOA_Week18_", nfl_scores2018$Year[i])))
  }
  else if (nfl_scores2018$Week[i] == 1 & nfl_scores2018$Year[i] == 2018){
    dvoa_df <- DVOA_Week18_2017
  }
  else if (nfl_scores2018$Week[i] == 1 & nfl_scores2018$Year[i] == 2017){
    dvoa_df <- dvoa2016
  }
  
  key_dvoa <- dvoa_df[match(nfl_scores2018$`Away Team`[i], dvoa_df$TEAM),]
  colnames(key_dvoa) <- paste0("away_", colnames(key_dvoa))
  
  key_dvoa_home <- dvoa_df[match(nfl_scores2018$`Home Team`[i], dvoa_df$TEAM),]
  colnames(key_dvoa_home) <- paste0("home_", colnames(key_dvoa_home))
  
  important_row <- cbind(nfl_scores2018[i,], key_dvoa, key_dvoa_home)
  nfl_scores_with_dvoa[i,] <- important_row
  
}

away_col <- paste0("away_", c("tot.rank", "team", "total.dvoa", "lw.rank", "WEI.dvoa", 
                              "WEI.rank", "W.L", "offense.dvoa", "offense.rank", "defense.dvoa",
                              "defense.rank", "st.dvoa", "st.rank"))
home_col <- paste0("home_", c("tot.rank", "team", "total.dvoa", "lw.rank", "WEI.dvoa",
                              "WEI.rank", "W.L", "offense.dvoa", "offense.rank", "defense.dvoa",
                              "defense.rank", "st.dvoa", "st.rank"))

colnames(nfl_scores_with_dvoa) <- c(colnames(nfl_scores2018), away_col, home_col)

home_wins_str <- substr(nfl_scores_with_dvoa$home_W.L, 1, 2)

home_wins <- ifelse(sapply(home_wins_str, function (x) "-" %in% substr(x, 2, 2)) == TRUE, 
                    substr(home_wins_str, 1, 1),
                    substr(home_wins_str, 1, 2))

home_wins <- as.numeric(home_wins)

away_wins_str <- substr(nfl_scores_with_dvoa$away_W.L, 1, 2)

away_wins <- ifelse(sapply(away_wins_str, function (x) "-" %in% substr(x, 2, 2)) == TRUE, 
                    substr(away_wins_str, 1, 1),
                    substr(away_wins_str, 1, 2))

away_wins <- as.numeric(away_wins)

sapply(nfl_scores_with_dvoa$home_W.L, function (x) substr(x, 3,4))
nfl_scores_with_dvoa$home_W.L


home_loss_str <- c()
for (i in 1:length(home_wins)){
  home_loss_str[i] <- ifelse(home_wins[i] >= 10, substr(nfl_scores_with_dvoa$home_W.L[i], 4,5),
                             substr(nfl_scores_with_dvoa$home_W.L[i], 3,4))
}

home_loss <- as.numeric(gsub("-", "", home_loss_str))

away_loss_str <- c()
for (i in 1:length(away_wins)){
  away_loss_str[i] <- ifelse(away_wins[i] >= 10, substr(nfl_scores_with_dvoa$away_W.L[i], 4,5),
                             substr(nfl_scores_with_dvoa$away_W.L[i], 3,4))
}
away_loss <- as.numeric(gsub("-", "", away_loss_str))

nfl_scores_with_dvoa$home.wins <- home_wins
nfl_scores_with_dvoa$home.loss <- home_loss
nfl_scores_with_dvoa$away.wins <- away_wins
nfl_scores_with_dvoa$away.loss <- away_loss
nfl_scores_with_dvoa <- nfl_scores_with_dvoa %>% mutate(
  home.winpercent = home.wins / (home.wins + home.loss),
  away.winpercent = away.wins / (away.wins + away.loss)
)

nfl_scores_with_dvoa$Date <- df$Date

more_stat2 <- merge(nfl_scores_with_dvoa, df, by.x = c("Date", "gameID"), by.y = c("Date", "game"))
more_stat2$away.winpercent <- ifelse(is.nan(more_stat2$away.winpercent), 0, more_stat2$away.winpercent)
more_stat2$home.winpercent <- ifelse(is.nan(more_stat2$home.winpercent), 0, more_stat2$home.winpercent)

more_stat2$scaled_away_cash <- more_stat2$away_cash_bet / 10000
more_stat2$scaled_tic_num <- more_stat2$away_ticket_num / 100
more_stat2$cash_tic_diff <- more_stat2$away_cash_percent - more_stat2$away_ticket_percent

colnames(more_stat2) <- c(colnames(more_stat2)[1:2], "Home.Team", "Away.Team", "home_score", "away_score",
                          colnames(more_stat2)[7:8],"neutral_venue", "away_line_open", "away_line_close",
                          colnames(more_stat2)[12:ncol(more_stat2)])

more_stat2$away_defense.dvoa = as.numeric(gsub("%", "", more_stat2$away_defense.dvoa))
more_stat2$home_defense.dvoa = as.numeric(gsub("%", "", more_stat2$home_defense.dvoa))
more_stat2$away_offense.dvoa = as.numeric(gsub("%", "", more_stat2$away_offense.dvoa))
more_stat2$home_offense.dvoa = as.numeric(gsub("%", "", more_stat2$home_offense.dvoa))
more_stat2$home_total.dvoa = as.numeric(gsub("%", "", more_stat2$home_total.dvoa))
more_stat2$away_total.dvoa = as.numeric(gsub("%", "", more_stat2$away_total.dvoa))
more_stat2$away_WEI.dvoa = as.numeric(gsub("%", "", more_stat2$away_WEI.dvoa))
more_stat2$home_WEI.dvoa = as.numeric(gsub("%", "", more_stat2$home_WEI.dvoa))
more_stat2$away_st.dvoa = as.numeric(gsub("%", "", more_stat2$away_st.dvoa))
more_stat2$home_st.dvoa = as.numeric(gsub("%", "", more_stat2$home_st.dvoa))

more_stat2 <- more_stat2 %>% mutate(
  WEI_away.diff = away_WEI.dvoa - home_WEI.dvoa,
  total_away.diff = away_total.dvoa - home_total.dvoa,
  offense_away.diff = away_offense.dvoa - home_offense.dvoa,
  defense_away.diff = away_defense.dvoa - home_defense.dvoa,
  st_away.diff = away_st.dvoa - home_st.dvoa,
  score_diff_away = away_score - home_score
)

more_stat2$`Playoff Game?` <- ifelse(is.na(more_stat2$`Playoff Game?`), 0, 1)
more_stat2$`Overtime?` <- ifelse(is.na(more_stat2$`Overtime?`), 0, 1)
more_stat2$neutral_venue <- ifelse(is.na(more_stat2$neutral_venue), 0, 1)

more_stat3 <- more_stat2[complete.cases(more_stat2),]
first_decision_point_spread <- c()

for (i in 1:nrow(more_stat3)){
  df <- eval(parse(text = paste0("df_", more_stat3$gameID[i], "_Week", more_stat3$Week[i], "_",
                                 more_stat3$Year[i])))
  h <- round(nrow(df) / 3)
  df_for_decision <- df[1:(nrow(df) - h + 1),]
  df_after_decision <- df[(nrow(df) - h):nrow(df),]
  first_decision_point_spread[i] <- as.numeric(tail(df_for_decision$spread, n=1))}

more_stat3$first_decision_point_spread <- first_decision_point_spread