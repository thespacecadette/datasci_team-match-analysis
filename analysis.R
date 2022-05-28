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


# The winner of each match has highest score.  
# Each team scores 3 points for a win, 1 point for a draw.
# Team with most points at the end of season is the season winner. 
teams_associatedPoints <- data.frame(matrix(ncol=2, nrow=0))
headings <- c("teamName", "totalPoints")
colnames(teams_associatedPoints) <- headings

# I built a function and passed a set (dataframe) and n (first n in set) variable
buildResultsLadder <- function(set, n_matches) {
  # aggregate points for each unique team in set
  for(i in 1:n_matches) {
    match <- set[i,]
    
    # debugger
    # print(match[["awayTeam"]])
    # print(awayTeamFinalPoints)
    # print(match[["homeTeam"]])
    # print(homeTeamFinalPoints)
    # print("___________________")
    # homeTeamFinalPoints <- 0
    # awayTeamFinalPoints <- 0
    
    # award points: win, draw
    if(match[["homeTeamPoints"]] > match[["awayTeamPoints"]]) {
      homeTeamFinalPoints <- match[["homeTeamPoints"]] + 3
      awayTeamFinalPoints <- match[["awayTeamPoints"]]
    } else if(match[["homeTeamPoints"]] == match[["awayTeamPoints"]]) {
      homeTeamFinalPoints <- match[["homeTeamPoints"]] + 1
      awayTeamFinalPoints <- match[["awayTeamPoints"]] + 1
    } else if(match[["homeTeamPoints"]] < match[["awayTeamPoints"]]){
      homeTeamFinalPoints <- match[["homeTeamPoints"]]
      awayTeamFinalPoints <- match[["awayTeamPoints"]] + 3
    } else {
      # do nothing
    }
    
    # aggregate team points
    homeTeamIdx <- which(teams_associatedPoints$teamName == match[["homeTeam"]])
    awayTeamIdx <- which(teams_associatedPoints$teamName == match[["awayTeam"]])
    
    if(length(homeTeamIdx) != 0) {
      teams_associatedPoints[homeTeamIdx, 1] <- match[["homeTeam"]]
      teams_associatedPoints[homeTeamIdx, 2] <- homeTeamFinalPoints
    } else {
      # get next row index
      nextRowIdx <- ifelse(length(teams_associatedPoints$teamName) == 0, 1, length(teams_associatedPoints$teamName) + 1)
      teams_associatedPoints[nextRowIdx, 1] <- match[["homeTeam"]]
      teams_associatedPoints[nextRowIdx, 2] <- homeTeamFinalPoints
    }
    
    if(length(awayTeamIdx) != 0) {
      teams_associatedPoints[awayTeamIdx, 1] <- match[["awayTeam"]]
      teams_associatedPoints[awayTeamIdx, 2] <- awayTeamFinalPoints
    } else {
      nextRowIdx <- ifelse(length(teams_associatedPoints$teamName) == 0, 1, length(teams_associatedPoints$teamName) + 1)
      teams_associatedPoints[nextRowIdx, 1] <- match[["awayTeam"]]
      teams_associatedPoints[nextRowIdx, 2] <- awayTeamFinalPoints
    }
    
  }
  # Order teams by points descending
  teams_associatedPoints_ordered <- order(teams_associatedPoints$totalPoints, decreasing = TRUE) # returns index of order, used to ORDER tables
  resultsLadder <- teams_associatedPoints[teams_associatedPoints_ordered,]
  print(resultsLadder)
}

# [RESULT] Results Ladder: Each team’s points as per the first 5 rounds (ordered by points)
first5MatchesSet <- seasonTable[1:5,]
first_n_matches <- length(first5MatchesSet$season)
buildResultsLadder(first5MatchesSet, first_n_matches)

# [RESULT] Results Ladder: Each team’s points for the full season (ordered by points)
season2016Set <- subset(seasonTable, subset=(season == 2016))
season_no_matches <- length(season2016Set$season)
buildResultsLadder(season2016Set, season_no_matches)

# Pick the season who’s last digit corresponds to 3.
season2013Set <- subset(seasonTable, subset=(season == 2013))
season_no_matches <- length(season2013Set$season)
buildResultsLadder(season2013Set, season_no_matches)


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












