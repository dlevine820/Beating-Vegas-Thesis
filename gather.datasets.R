#get dataset with consensus picks

nfl_scores <- read_xlsx("nfl.xlsx", sheet = "Data")
nfl_scores2018 <- nfl_scores[as.numeric(format(nfl_scores$Date,'%Y')) == 2018 | 
                               as.numeric(format(nfl_scores$Date,'%Y')) == 2017 
                             & as.numeric(format(nfl_scores$Date,'%m')) >= 3, c(1:8, 21, 24)]
nfl_teams <- read.csv("nfl_teams.csv")
nfl_teams$Name <- gsub("NY", "New York", nfl_teams$Name)
nfl_teams$Name <- gsub("St. Louis", "Los Angeles", nfl_teams$Name)
nfl_teams$Name <- gsub("San Diego", "Los Angeles", nfl_teams$Name)
nfl_teams$Abbreviation <- gsub("STL", "LAR", nfl_teams$Abbreviation)
nfl_teams$Abbreviation <- gsub("SD", "LAC", nfl_teams$Abbreviation)
nfl_teams$Abbreviation <- gsub("JAX", "JAC", nfl_teams$Abbreviation)
nfl_scores2018$`Home Team` <- nfl_teams$Abbreviation[match(nfl_scores2018$`Home Team`, nfl_teams$Name)]
nfl_scores2018$`Away Team` <- nfl_teams$Abbreviation[match(nfl_scores2018$`Away Team`, nfl_teams$Name)]
nfl_scores2018 <- nfl_scores2018[!is.na(nfl_scores2018$`Home Team`),]
nfl_scores2018$away_team_cover <- ifelse(nfl_scores2018$`Away Score` + nfl_scores2018$`Away Line Close` > nfl_scores2018$`Home Score`, 1,0)

list_of_games <- paste(nfl_scores2018$`Away Team`, nfl_scores2018$`Home Team`, sep = "v")

week_numbers <- c(rep(10, 14), rep(9, 13), rep(8:7, each = 14), rep(6:4, each=15), rep(3:1, each = 16),
                  rep(0, 11), rep(17:12, each = 16), rep(11, 14), rep(10, 14), rep(9:8, each = 13),
                  rep(7, 15), rep(6:5, each = 14), rep(4:2, each = 16), rep(1, 15))
nfl_scores2018$Week <- week_numbers
nfl_scores2018$Year <- c(rep(2018, 148), rep(2017, nrow(nfl_scores2018) - 148))

right = function (string, char){
  substr(string,nchar(string)-(char-1),nchar(string))}

left = function (string,char){
  substr(string,1,char)}


df <- data.frame()
for (i in 1:nrow(nfl_scores2018)){
  a <- read_xlsx(paste0(nfl_scores2018$Year[i], "/Week", nfl_scores2018$Week[i], ".xlsx"), sheet = list_of_games[i])
  colnames(a) <- c("Date", "Time", "Odds", "away_cash_bet", "away_cash_percent", "away_ticket_num",
                   "away_ticket_percent", "away_pick_num", "away_pick_percent")
  a$game <- list_of_games[i]
  a$week <- week_numbers[i]
  a$Odds <- gsub("pk", 0, a$Odds)
  a$Odds <- gsub("ev", 100, a$Odds)
  a$Odds <- gsub("½", .5, a$Odds)
  a$away_cash_percent <- ifelse(a$away_cash_percent == 0, 0.001, a$away_cash_percent)
  a$away_ticket_percent <- ifelse(a$away_ticket_percent == 0, 0.001, a$away_ticket_percent)
  a$spread <- ifelse(abs(as.numeric(right(a$Odds, 4))) >= 100, 
                     left(a$Odds, nchar(a$Odds) - 4),
                     a$Odds)
  a$odds <- ifelse(abs(as.numeric(right(a$Odds, 4))) >= 100, 
                   right(a$Odds, 4),
                   "-110")
  a$log_away_cash_bet <- log(a$away_cash_bet + 1)
  a$log_away_tic_num <- log(a$away_ticket_num + 1)
  a$home_cash_bet <- (a$away_cash_bet / a$away_cash_percent) - a$away_cash_bet 
  a$home_ticket_num <- (a$away_ticket_num / a$away_ticket_percent) - a$away_ticket_num
  a$log_home_cash_bet <- log(a$home_cash_bet + 1)
  a$log_home_tic_num <- log(a$home_ticket_num + 1)
  df <- rbind(df, a[1,])
  assign(paste0("df_", list_of_games[i], "_Week", nfl_scores2018$Week[i], "_",
                nfl_scores2018$Year[i]), a[rev(rownames(a)),])
}

nfl_scores2018$gameID <- paste(nfl_scores2018$`Away Team`, nfl_scores2018$`Home Team`, sep = "v")