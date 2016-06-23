library("data.table")
library("rvest")
library("stringr")

print("Loading projections from ESPN")

positions <- c(QB=0, RB=2, WR=4, TE=6)
pages <- seq(0, 200, 40)
base_url <- "http://games.espn.go.com/ffl/tools/projections"

urls <- paste0(base_url, "?&slotCategoryId=", rep(positions, each=length(pages)), "&startIndex=", pages)
espn_projections <- lapply(urls, function(x) { read_html(x) %>% html_node("#playertable_0") %>% html_table() })
espn_projections <- lapply(espn_projections, FUN = function(x) { x[-1,] })
espn_projections <- rbindlist(espn_projections, use.names = TRUE, fill = TRUE)

column_names <-
  c("RNK", "PLAYER",
    "passing.C/A", "passing.YDS", "passing.TD", "passing.INT",
    "rushing.RUSH", "rushing.YDS", "rushing.TD",
    "receiving.REC", "receiving.YDS", "receiving.TD",
    "PTS")
colnames(espn_projections) <- column_names

# Clean up player names
player_team_pos <- str_split(espn_projections$PLAYER, ",")
player_names <- sapply(player_team_pos, "[[", 1)
team_pos <- sapply(player_team_pos, "[[", 2)
team_pos <- str_split(str_replace_all(str_trim(team_pos), "Â", " "), " ")
teams <- str_trim(sapply(team_pos, "[[", 1))
pos <- str_trim(sapply(team_pos, "[[", 2))

espn_projections$PLAYER <- player_names
espn_projections <- cbind(espn_projections, POS=pos)
espn_projections <- cbind(espn_projections, TEAM=teams)

espn_projections$RNK <- as.numeric(espn_projections$RNK)

# Replace empty stats with zeros
espn_projections$`passing.C/A` <- str_replace(espn_projections$`passing.C/A`, "--/--", "0/0")

comp <- sapply(str_split(espn_projections$`passing.C/A`, "/"), "[[", 1) %>% as.numeric()
espn_projections <- cbind(espn_projections, `passing.C` = comp)

attmpt <- sapply(str_split(espn_projections$`passing.C/A`, "/"), "[[", 2) %>% as.numeric()
espn_projections <- cbind(espn_projections, `passing.A` = attmpt)

as.numeric_handle_dash <- function(x) {
  return(x %>% str_replace("--", "0") %>% as.numeric())
}

espn_projections$passing.YDS <- as.numeric_handle_dash(espn_projections$passing.YDS)
espn_projections$passing.TD <- as.numeric_handle_dash(espn_projections$passing.TD)
espn_projections$passing.INT <- as.numeric_handle_dash(espn_projections$passing.INT)

espn_projections$rushing.RUSH <- as.numeric_handle_dash(espn_projections$rushing.RUSH)
espn_projections$rushing.YDS <- as.numeric_handle_dash(espn_projections$rushing.YDS)
espn_projections$rushing.TD <- as.numeric_handle_dash(espn_projections$rushing.TD)

espn_projections$receiving.REC <- as.numeric_handle_dash(espn_projections$receiving.REC)
espn_projections$receiving.YDS <- as.numeric_handle_dash(espn_projections$receiving.YDS)
espn_projections$receiving.TD <- as.numeric_handle_dash(espn_projections$receiving.TD)

# Factor the POS
espn_projections$POS <- as.factor(espn_projections$POS)
espn_projections$TEAM <- as.factor(espn_projections$TEAM)

league_rules <-
  c(passing.YDS.per.pt = 25, passing.TD.pt = 4, passing.INT.pt = -2,
    rushing.YDS.per.pt = 10, rushing.TD.pt = 6,
    receiving.YDS.per.pt = 10, receiving.TD.pt = 6, receiving.REC.pt = 1,
    starting.QB = 1, starting.RB = 2, starting.WR = 2, starting.TE = 1,
    teams.in.league = 10)

espn_projections$PTS <-
  (espn_projections$passing.YDS / league_rules["passing.YDS.per.pt"]) +
  (espn_projections$passing.TD * league_rules["passing.TD.pt"]) +
  (espn_projections$passing.INT * league_rules["passing.INT.pt"]) +
  (espn_projections$rushing.YDS / league_rules["rushing.YDS.per.pt"]) +
  (espn_projections$rushing.TD * league_rules["rushing.TD.pt"]) +
  (espn_projections$receiving.REC * league_rules["receiving.REC.pt"]) +
  (espn_projections$receiving.YDS / league_rules["receiving.YDS.per.pt"]) +
  (espn_projections$receiving.TD * league_rules["receiving.TD.pt"])

final_projections <- data.frame(matrix(vector(), 0, length(colnames(espn_projections))+1))
colnames(final_projections) <- colnames(c("RNK", "PLAYER",
                                          "passing.C/A", "passing.YDS", "passing.TD", "passing.INT",
                                          "rushing.RUSH", "rushing.YDS", "rushing.TD",
                                          "receiving.REC", "receiving.YDS", "receiving.TD",
                                          "PTS", "CVP"))

for(pos in c("QB", "RB", "WR", "TE")) {
  pos_projections <- espn_projections[POS == pos]
  pos_projections <- pos_projections[order(-pos_projections$PTS)]

  key <- paste0("starting.", pos)
  last_starter <- pos_projections[league_rules[key] * league_rules["teams.in.league"],]
  cpv <- pos_projections$PTS - last_starter$PTS
  pos_projections <- cbind(pos_projections, CVP = cpv)
  final_projections <- rbind(final_projections, pos_projections)
}

espn_projections <- final_projections[order(-final_projections$CVP)]
espn_projections <- subset(espn_projections, select = -c(RNK, `passing.C/A`, passing.C, passing.A))

rm(final_projections, last_starter, pos_projections, attmpt, base_url, column_names,
   comp, cpv, key, pages, player_names, player_team_pos, pos, positions, team_pos, teams, urls,
   as.numeric_handle_dash)
