df <- read.csv("competitionResults.csv")

# DATA PREPROCESSING
head(df)

# [RESULT] Total scored by each team for each match. Each goal is worth 10 points, team that caught the snitch  awarded extra 50 points. 
# * the season and round number
# * the match number (a new variable that identifies each match of the season)
# * the home and away team names and their scores
seasonTable <- data.frame(matrix(ncol=7, nrow=0))
headings <- c("season", "round", "matchNo", "homeTeam", "homeTeamPoints", "awayTeam", "awayTeamPoints")
colnames(seasonTable) <- headings
print(seasonTable)


for (match_number in 1:nrow(df)) {
  match <- df[match_number,]
  totalPointsHomeTeam <- match[["homeGoals"]] * 10 + ifelse(match[["homeTeam"]] == match[["snitch"]], 50, 0)
  totalPointsAwayTeam <- match[["awayGoals"]] * 10 + ifelse(match[["awayTeam"]] == match[["snitch"]], 50, 0)

  seasonTable[match_number, 1] <- match[["Season"]]
  seasonTable[match_number, 2] <- match[["round"]]
  seasonTable[match_number, 3] <- match_number
  seasonTable[match_number, 4] <- match[["homeTeam"]]
  seasonTable[match_number, 5] <- totalPointsHomeTeam
  seasonTable[match_number, 6] <- match[["awayTeam"]]
  seasonTable[match_number, 7] <- totalPointsAwayTeam
}

head(seasonTable)
# Team with most points at the end of season is the season winner. 
# TODO: [RESULT] Each team’s points as per the first 5 rounds of matches and full season. 
# TODO: Season Results [TABLE] (two tables, one table for each set of results),
# TODO: ordering teams by their points (i.e. a results ladder). 

# Pick the season who’s last digit corresponds to 3.


# An investigation into home game advantage is planned for the coming months. 
# [RESULTS] Return: Count of 
# * The number of home games each team has had
# * The total number of games each team has played in
# * The total points scored in a home game and the total number of points the team has scored. 
# * Provide these results in a data frame. Run the function on the provided data and show the resulting table output.


# Provide a table containing the team statistics for all seasons. 
# Including, but not limited to the following:
# * Number of games
# * Number of wins
# * Winning percentage, points scored, points conceded, no of snitch catches, goals scored, number of tournament wins. 
# Teams ordered in terms of total points scored.












